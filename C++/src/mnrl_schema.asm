; http://csl.sublevel3.org/post/embedding-binary-data/
bits 64

section .rodata

global _binary_mnrl_schema_json_start
global _binary_mnrl_schema_json_size
global _binary_mnrl_schema_json_end

_binary_mnrl_schema_json_start:  incbin "../mnrl-schema.json"
_binary_mnrl_schema_json_end:
_binary_mnrl_schema_json_size:   dd $-_binary_mnrl_schema_json_start
