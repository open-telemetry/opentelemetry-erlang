

%% The GraphQL document being executed.
-define(GRAPHQL_DOCUMENT, 'graphql.document').


%% The name of the operation being executed.
-define(GRAPHQL_OPERATION_NAME, 'graphql.operation.name').


%% The type of the operation being executed.

-define('graphql_operation_type.query', 'query').

-define('graphql_operation_type.mutation', 'mutation').

-define('graphql_operation_type.subscription', 'subscription').

-define(graphql_operation_type(Custom), Custom).
