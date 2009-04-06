{application, poa,
 [{description, "poa"},
  {vsn, "0.01"},
  {modules, [generic_tcp_server,
             intervals,
             poa,
             poa_app,
             poa_inbound
            ]},
  {registered, []},
  {mod, {poa_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
