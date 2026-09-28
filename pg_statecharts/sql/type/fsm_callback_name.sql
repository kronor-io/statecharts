create type fsm_callback_name as (
  schema_name name,
  function_name name
);

comment on type fsm_callback_name is
    'Type for on_entry and on_exit callbacks. Should contain both the schema and the function name to invoke';
