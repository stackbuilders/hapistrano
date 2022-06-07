# CI Workflows


```mermaid
sequenceDiagram
  participant Build
  participant Draft
  participant Release
  Build->>Draft: Tag created
  Draft->>Release: Publish release
```
