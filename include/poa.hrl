-record(poa_link_state, {sock, poa_pid, remote_addresses, remote_node_id}).

-define(OUTBOUND_CONNECTION_RETRY_DELAY_MS, 10000).
-define(STATE_BROADCAST_INTERVAL_MS, 100).
