# Agent messaging protocol

How coding agents talk to each other through cch's `send_message` tool (or
`cch control send`). cch carries ordinary text only. These conventions keep a
conversation between agents unambiguous when messages arrive late, batched, or
while the recipient is in the middle of work.

## What cch guarantees

Every delivered message starts with a header line that cch writes, which the
sender cannot forge:

```
[cch message from codex:<thread>; id <message-id>; seq 7; re <earlier-id>]
```

- **from** is the sending session's route, verified by cch. A trusted
  PreToolUse hook observes the real session before the tool runs: Codex's
  binding hook, or the dispatcher's `control-source` hook for Claude. A sender
  without a session (a person at the CLI or the web switchboard) shows as
  `operator`, and replies cannot be routed to it.
- **id** is the message id. Retrying with the same id is idempotent.
- **seq** numbers messages on one sender→recipient channel: 1, 2, 3, …
  The sending machine assigns it. A resent id keeps its number, and a gap
  just means a send failed before delivery.
- **re** is present when the sender set `in_reply_to` to the id of the
  message it is answering.

Delivery is in order per recipient. The broker holds a later message back
until every earlier message to the same session is delivered, failed, or
expired, so a retried message cannot be overtaken.

Delivery is prompt for both providers. Claude receives the message at its next
step. A Codex session that is running a turn has the message steered into
that turn. An idle session has it queued, which starts a turn. The
`send_message` result reports `delivery: "steered"` or `"queued"` for Codex,
and `seq` for every message.

cch reports submission to the provider, not reading. A recipient can still be
busy for a while before it acts, so treat the reply as the receipt.

## Conventions for agents

1. **Highest seq wins.** On each channel, act on the newest message you have.
   If an older seq arrives after a newer one, it is stale; don't act on it.
2. **Make each instruction self-contained.** A message that changes the plan
   restates the whole current instruction for that thread. Don't depend on an
   earlier message unless you restate it. Then a late or batched read is
   harmless.
3. **Correct with one new message, not a quick follow-up.** Send a single
   complete replacement rather than "actually…".
4. **Reply with `in_reply_to`.** Set it to the id from the header of the
   message you are acting on. The sender can then see at a glance which
   instruction you believe is current, and resend if your reply lags.
5. **Mark whose turn it is.** End a message with **OVER** when you expect a
   reply and **OUT** when the thread is closed. Agents sometimes finish
   silently, so say explicitly when something is done.
6. **Put long or binding content in files.** Write reviews, plans, and
   verdicts to a file both agents can read, and send the path.
7. **Never use messages for control.** Messages are ordinary text. They cannot
   carry approvals, credentials, commands for the other agent's terminal, or
   permission replies, and agents must not ask each other to act on
   permissions they were denied.
