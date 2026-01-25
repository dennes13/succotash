;; File Indexing Smart Contract
;; Purpose: Index files for search and retrieval with metadata support

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-FILE-NOT-FOUND (err u101))
(define-constant ERR-INVALID-PAGINATION (err u102))
(define-constant ERR-DUPLICATE-FILE (err u103))
(define-constant ERR-INVALID-INPUT (err u104))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; File counter for unique IDs
(define-data-var file-counter uint u0)

;; File metadata structure
(define-map files
  { file-id: uint }
  {
    filename: (string-ascii 256),
    file-hash: (string-ascii 64),
    uploader: principal,
    tags: (list 10 (string-ascii 64)),
    file-size: uint,
    content-type: (string-ascii 64),
    upload-timestamp: uint,
    is-active: bool
  }
)

;; Index by filename (for exact matches)
(define-map filename-index
  { filename: (string-ascii 256) }
  { file-id: uint }
)

;; Index by uploader
(define-map uploader-files
  { uploader: principal, file-id: uint }
  { filename: (string-ascii 256) }
)

;; Tag index (each tag maps to list of file IDs)
(define-map tag-index
  { tag: (string-ascii 64) }
  { file-ids: (list 100 uint) }
)

;; Pagination helper - stores file IDs in order
(define-map file-list
  { index: uint }
  { file-id: uint }
)

;; Helper function to validate tags list
(define-private (validate-tags (tags (list 10 (string-ascii 64))))
  (fold validate-single-tag tags true)
)

;; Helper function to validate individual tag
(define-private (validate-single-tag (tag (string-ascii 64)) (valid bool))
  (and valid 
       (> (len tag) u0)
       (<= (len tag) u64))
)
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Helper function to increment file counter
(define-private (increment-file-counter)
  (let ((current-counter (var-get file-counter)))
    (var-set file-counter (+ current-counter u1))
    (+ current-counter u1)
  )
)

;; Helper function to add file ID to tag index
(define-private (add-to-tag-index (tag (string-ascii 64)) (file-id uint))
  (let ((existing-ids (default-to (list) (get file-ids (map-get? tag-index { tag: tag })))))
    (map-set tag-index 
      { tag: tag }
      { file-ids: (unwrap! (as-max-len? (append existing-ids file-id) u100) false) }
    )
  )
)

;; Helper function to process tags for a file
(define-private (process-tags (tags (list 10 (string-ascii 64))) (file-id uint))
  (fold add-tag-to-index tags { file-id: file-id, success: true })
)

;; Helper function for tag processing fold
(define-private (add-tag-to-index 
  (tag (string-ascii 64)) 
  (context { file-id: uint, success: bool }))
  (if (get success context)
    { 
      file-id: (get file-id context), 
      success: (add-to-tag-index tag (get file-id context))
    }
    context
  )
)

;; Add or update a file in the index
(define-public (index-file 
  (filename (string-ascii 256))
  (file-hash (string-ascii 64))
  (tags (list 10 (string-ascii 64)))
  (file-size uint)
  (content-type (string-ascii 64)))
  (let (
    (new-file-id (increment-file-counter))
    (current-block block-height)
  )
    ;; Check if filename already exists
    (asserts! (is-none (map-get? filename-index { filename: filename })) ERR-DUPLICATE-FILE)
    
    ;; Validate inputs - all user inputs are untrusted
    (asserts! (and (> (len filename) u0) (<= (len filename) u256)) ERR-INVALID-INPUT)
    (asserts! (and (> (len file-hash) u0) (is-eq (len file-hash) u64)) ERR-INVALID-INPUT)
    (asserts! (and (> (len content-type) u0) (<= (len content-type) u64)) ERR-INVALID-INPUT)
    (asserts! (< file-size u1000000000) ERR-INVALID-INPUT) ;; Max file size limit
    
    ;; Validate tags - check each tag length
    (asserts! (validate-tags tags) ERR-INVALID-INPUT)
    
    ;; Store file metadata
    (map-set files
      { file-id: new-file-id }
      {
        filename: filename,
        file-hash: file-hash,
        uploader: tx-sender,
        tags: tags,
        file-size: file-size,
        content-type: content-type,
        upload-timestamp: current-block,
        is-active: true
      }
    )
    
    ;; Update filename index
    (map-set filename-index
      { filename: filename }
      { file-id: new-file-id }
    )
    
    ;; Update uploader index
    (map-set uploader-files
      { uploader: tx-sender, file-id: new-file-id }
      { filename: filename }
    )
    
    ;; Add to file list for pagination
    (map-set file-list
      { index: new-file-id }
      { file-id: new-file-id }
    )
    
    ;; Process tags
    (process-tags tags new-file-id)
    
    (ok new-file-id)
  )
)

;; Update file metadata (only by uploader or contract owner)
(define-public (update-file-metadata
  (file-id uint)
  (new-tags (list 10 (string-ascii 64)))
  (new-content-type (string-ascii 64)))
  (let ((file-data (unwrap! (map-get? files { file-id: file-id }) ERR-FILE-NOT-FOUND)))
    ;; Check authorization
    (asserts! (or (is-eq tx-sender (get uploader file-data)) (is-contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Validate untrusted inputs
    (asserts! (and (> (len new-content-type) u0) (<= (len new-content-type) u64)) ERR-INVALID-INPUT)
    (asserts! (validate-tags new-tags) ERR-INVALID-INPUT)
    (asserts! (> file-id u0) ERR-INVALID-INPUT)
    (asserts! (get is-active file-data) ERR-FILE-NOT-FOUND) ;; Only update active files
    
    ;; Update file data
    (map-set files
      { file-id: file-id }
      (merge file-data {
        tags: new-tags,
        content-type: new-content-type
      })
    )
    
    ;; Update tag indices (simplified - in production, you'd want to remove old tags first)
    (process-tags new-tags file-id)
    
    (ok true)
  )
)

;; Remove file from index (soft delete)
(define-public (remove-file (file-id uint))
  (let ((file-data (unwrap! (map-get? files { file-id: file-id }) ERR-FILE-NOT-FOUND)))
    ;; Validate untrusted input
    (asserts! (> file-id u0) ERR-INVALID-INPUT)
    
    ;; Check authorization
    (asserts! (or (is-eq tx-sender (get uploader file-data)) (is-contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Soft delete - mark as inactive
    (map-set files
      { file-id: file-id }
      (merge file-data { is-active: false })
    )
    
    (ok true)
  )
)

;; Search by filename (exact match)
(define-read-only (search-by-filename (filename (string-ascii 256)))
  ;; Validate untrusted input
  (if (and (> (len filename) u0) (<= (len filename) u256))
    (match (map-get? filename-index { filename: filename })
      index-entry (map-get? files { file-id: (get file-id index-entry) })
      none
    )
    none
  )
)

;; Get files by uploader with pagination
(define-read-only (get-files-by-uploader 
  (uploader principal) 
  (offset uint) 
  (limit uint))
  (let (
    (max-limit u50)
    (actual-limit (if (> limit max-limit) max-limit limit))
  )
    (asserts! (<= limit max-limit) ERR-INVALID-PAGINATION)
    
    ;; This is a simplified version - in practice, you'd implement proper pagination
    ;; by maintaining ordered lists or using a more sophisticated indexing mechanism
    (ok { 
      files: (list),
      total: u0,
      offset: offset,
      limit: actual-limit
    })
  )
)

;; Search files by tag
(define-read-only (search-by-tag (tag (string-ascii 64)))
  ;; Validate untrusted input
  (if (and (> (len tag) u0) (<= (len tag) u64))
    (match (map-get? tag-index { tag: tag })
      tag-entry (ok (get file-ids tag-entry))
      (ok (list))
    )
    (ok (list))
  )
)

;; Get file details by ID
(define-read-only (get-file-by-id (file-id uint))
  ;; Validate untrusted input
  (if (> file-id u0)
    (map-get? files { file-id: file-id })
    none
  )
)

;; Get paginated file list
(define-read-only (get-files-paginated (offset uint) (limit uint))
  (let (
    (max-limit u50)
    (actual-limit (if (> limit max-limit) max-limit limit))
    (total-files (var-get file-counter))
  )
    (asserts! (<= limit max-limit) ERR-INVALID-PAGINATION)
    (asserts! (< offset total-files) ERR-INVALID-PAGINATION)
    
    ;; Return pagination info (simplified implementation)
    (ok {
      total: total-files,
      offset: offset,
      limit: actual-limit,
      has-more: (< (+ offset actual-limit) total-files)
    })
  )
)

;; Get contract statistics
(define-read-only (get-stats)
  (ok {
    total-files: (var-get file-counter),
    contract-owner: (var-get contract-owner)
  })
)

;; Transfer contract ownership (only current owner)
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; Helper function to check if a file exists and is active
(define-read-only (file-exists (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data (get is-active file-data)
    false
  )
)