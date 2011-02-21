-ifndef(CT_AUTOEXPORT_HRL).
-define(CT_AUTOEXPORT_HRL, true).

%% Parse transform for automatic exporting of test functions.

-ifndef(CT_NOAUTOEXPORT).
-ifndef(line).  %% have no effect if included after test_server.hrl/ct.hrl
-compile({parse_transform, ct_autoexport}).
-endif.
-endif.

-endif. % CT_AUTOEXPORT_HRL
