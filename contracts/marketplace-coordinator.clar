;; Multi-vendor Marketplace Platform
;; Coordinates sellers, products, orders, and payment distribution

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-stock (err u104))
(define-constant err-invalid-status (err u105))

;; Define data variables
(define-data-var next-vendor-id uint u1)
(define-data-var next-product-id uint u1)
(define-data-var next-order-id uint u1)
(define-data-var marketplace-fee-percent uint u3)

;; Define data structures
(define-map vendors
  { vendor-id: uint }
  {
    owner: principal,
    business-name: (string-ascii 100),
    category: (string-ascii 50),
    description: (string-ascii 300),
    verified: bool,
    rating: uint,
    total-sales: uint,
    active: bool
  }
)

(define-map products
  { product-id: uint }
  {
    vendor-id: uint,
    name: (string-ascii 100),
    description: (string-ascii 500),
    price: uint,
    stock-quantity: uint,
    category: (string-ascii 50),
    featured: bool,
    active: bool
  }
)

(define-map orders
  { order-id: uint }
  {
    buyer: principal,
    product-id: uint,
    vendor-id: uint,
    quantity: uint,
    total-amount: uint,
    status: (string-ascii 20),
    order-date: uint,
    shipping-address: (string-ascii 200)
  }
)

(define-map vendor-stats
  { vendor-id: uint }
  {
    total-orders: uint,
    total-revenue: uint,
    average-rating: uint,
    product-count: uint
  }
)

(define-map payment-distributions
  { order-id: uint }
  {
    vendor-amount: uint,
    marketplace-fee: uint,
    distributed: bool,
    distribution-date: uint
  }
)

;; Register vendor
(define-public (register-vendor (business-name (string-ascii 100)) (category (string-ascii 50)) (description (string-ascii 300)))
  (let ((vendor-id (var-get next-vendor-id)))
    (map-set vendors
      { vendor-id: vendor-id }
      {
        owner: tx-sender,
        business-name: business-name,
        category: category,
        description: description,
        verified: false,
        rating: u0,
        total-sales: u0,
        active: true
      }
    )
    (map-set vendor-stats
      { vendor-id: vendor-id }
      {
        total-orders: u0,
        total-revenue: u0,
        average-rating: u0,
        product-count: u0
      }
    )
    (var-set next-vendor-id (+ vendor-id u1))
    (ok vendor-id)
  )
)

;; Add product
(define-public (add-product (vendor-id uint) (name (string-ascii 100)) (description (string-ascii 500)) (price uint) (stock-quantity uint) (category (string-ascii 50)))
  (let ((product-id (var-get next-product-id))
        (vendor (unwrap! (map-get? vendors { vendor-id: vendor-id }) err-not-found)))
    (asserts! (is-eq (get owner vendor) tx-sender) err-unauthorized)
    (asserts! (get active vendor) err-invalid-status)
    (map-set products
      { product-id: product-id }
      {
        vendor-id: vendor-id,
        name: name,
        description: description,
        price: price,
        stock-quantity: stock-quantity,
        category: category,
        featured: false,
        active: true
      }
    )
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

;; Place order
(define-public (place-order (product-id uint) (quantity uint) (shipping-address (string-ascii 200)))
  (let ((order-id (var-get next-order-id))
        (product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
        (total-amount (* (get price product) quantity)))
    (asserts! (get active product) err-invalid-status)
    (asserts! (<= quantity (get stock-quantity product)) err-insufficient-stock)
    (map-set orders
      { order-id: order-id }
      {
        buyer: tx-sender,
        product-id: product-id,
        vendor-id: (get vendor-id product),
        quantity: quantity,
        total-amount: total-amount,
        status: "pending",
        order-date: stacks-block-height,
        shipping-address: shipping-address
      }
    )
    (map-set products
      { product-id: product-id }
      (merge product { stock-quantity: (- (get stock-quantity product) quantity) })
    )
    (var-set next-order-id (+ order-id u1))
    (ok order-id)
  )
)

;; Update order status
(define-public (update-order-status (order-id uint) (status (string-ascii 20)))
  (let ((order (unwrap! (map-get? orders { order-id: order-id }) err-not-found))
        (vendor (unwrap! (map-get? vendors { vendor-id: (get vendor-id order) }) err-not-found)))
    (asserts! (is-eq (get owner vendor) tx-sender) err-unauthorized)
    (map-set orders
      { order-id: order-id }
      (merge order { status: status })
    )
    (ok true)
  )
)

;; Distribute payment
(define-public (distribute-payment (order-id uint))
  (let ((order (unwrap! (map-get? orders { order-id: order-id }) err-not-found))
        (marketplace-fee (/ (* (get total-amount order) (var-get marketplace-fee-percent)) u100))
        (vendor-amount (- (get total-amount order) marketplace-fee)))
    (asserts! (is-eq contract-owner tx-sender) err-owner-only)
    (map-set payment-distributions
      { order-id: order-id }
      {
        vendor-amount: vendor-amount,
        marketplace-fee: marketplace-fee,
        distributed: true,
        distribution-date: stacks-block-height
      }
    )
    (ok vendor-amount)
  )
)

;; Verify vendor
(define-public (verify-vendor (vendor-id uint))
  (let ((vendor (unwrap! (map-get? vendors { vendor-id: vendor-id }) err-not-found)))
    (asserts! (is-eq contract-owner tx-sender) err-owner-only)
    (map-set vendors
      { vendor-id: vendor-id }
      (merge vendor { verified: true })
    )
    (ok true)
  )
)

;; Feature product
(define-public (feature-product (product-id uint))
  (let ((product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
        (vendor (unwrap! (map-get? vendors { vendor-id: (get vendor-id product) }) err-not-found)))
    (asserts! (is-eq (get owner vendor) tx-sender) err-unauthorized)
    (map-set products
      { product-id: product-id }
      (merge product { featured: true })
    )
    (ok true)
  )
)

;; Update stock
(define-public (update-stock (product-id uint) (new-quantity uint))
  (let ((product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
        (vendor (unwrap! (map-get? vendors { vendor-id: (get vendor-id product) }) err-not-found)))
    (asserts! (is-eq (get owner vendor) tx-sender) err-unauthorized)
    (map-set products
      { product-id: product-id }
      (merge product { stock-quantity: new-quantity })
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-vendor (vendor-id uint))
  (map-get? vendors { vendor-id: vendor-id })
)

(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-order (order-id uint))
  (map-get? orders { order-id: order-id })
)

(define-read-only (get-vendor-stats (vendor-id uint))
  (map-get? vendor-stats { vendor-id: vendor-id })
)

(define-read-only (get-payment-distribution (order-id uint))
  (map-get? payment-distributions { order-id: order-id })
)

(define-read-only (get-total-vendors)
  (- (var-get next-vendor-id) u1)
)

(define-read-only (get-total-products)
  (- (var-get next-product-id) u1)
)

(define-read-only (get-marketplace-fee)
  (var-get marketplace-fee-percent)
)


;; title: marketplace-coordinator
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

