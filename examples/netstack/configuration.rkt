#lang racket/base

(provide (struct-out ethernet-interface)
	 (struct-out host-route)
	 (struct-out gateway-route)
	 (struct-out net-route)

         (struct-out route-up))

(struct ethernet-interface (name hwaddr) #:prefab)

;; A Route is one of
;;  - (host-route IpAddrBytes NetmaskNat InterfaceName), an own-IP route
;;  - (gateway-route NetAddrBytes NetmaskNat IpAddrBytes InterfaceName), a gateway for a subnet
;;  - (net-route NetAddrBytes NetmaskNat InterfaceName), an ethernet route for a subnet
;; NetmaskNat in a net-route is a default route.
(struct host-route (ip-addr netmask interface-name) #:prefab)
(struct gateway-route (network-addr netmask gateway-addr interface-name) #:prefab)
(struct net-route (network-addr netmask link) #:prefab)

(struct route-up (route) #:prefab) ;; assertion: the given Route is running
