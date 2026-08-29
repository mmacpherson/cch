(ns cch.control.broker-api
  "Storage-independent operations used by the private broker HTTP boundary.")

(defprotocol ControlBroker
  (authorize-runner! [broker runner-id token])
  (register-runner! [broker request])
  (active-sessions [broker])
  (set-session-alias! [broker request])
  (set-operator-session-alias! [broker request])
  (enqueue-message! [broker request])
  (enqueue-operator-message! [broker request])
  (poll-messages! [broker request])
  (ack-message! [broker request])
  (publish-usage-observations! [broker request])
  (read-usage-observations! [broker request])
  (message-metadata [broker runner-id token message-id])
  (operator-message-metadata [broker message-id])
  (broker-summary [broker])
  (close-broker! [broker]))
