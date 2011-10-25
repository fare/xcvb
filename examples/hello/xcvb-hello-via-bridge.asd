(defsystem :xcvb-hello-via-bridge
  :defsystem-depends-on (:xcvb-bridge)
  :class :xcvb-build
  :build "/xcvb/hello")
