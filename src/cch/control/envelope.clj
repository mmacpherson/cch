(ns cch.control.envelope
  "The provenance header cch prepends to every delivered text message.

  Native adapters share one format so a recipient can read the sender, the
  message id, the sender->recipient channel sequence, and the message being
  answered the same way whichever provider delivered it.")

(defn header
  "Render the header line for one delivery. seq and in-reply-to are optional;
  envelopes accepted before sequencing existed carry neither."
  [{:keys [source message-id seq in-reply-to]}]
  (str "[cch message from " source
       "; id " message-id
       (when seq (str "; seq " seq))
       (when in-reply-to (str "; re " in-reply-to))
       "]"))

(defn render
  "Return the text a native inbox receives: the header line, then the body."
  [{:keys [message] :as envelope}]
  (str (header envelope) "\n" message))
