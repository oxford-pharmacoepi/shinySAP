# Changelog

## shinySAP 0.0.0.9000

- The Shiny app now lives in `inst/app` and is launched with
  [`shinySap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/shinySap.md).
  It reads and writes the package’s own `sap` object; the app-private
  JSON schema (history below) is retired.
- New JSON layer:
  [`sapToJson()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/sapToJson.md),
  [`sapFromJson()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/sapFromJson.md),
  [`writeSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/writeSap.md),
  [`readSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/readSap.md).
- Exported read-only schema accessors:
  [`sapSchemaVersion()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/sapSchemaVersion.md),
  [`sapSchemaTypes()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/sapSchemaTypes.md),
  [`sapSchemaFields()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/sapSchemaFields.md),
  [`sapSchemaVocabulary()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/sapSchemaVocabulary.md).
- `constructSap()` passed the schema version in the `typeId` slot of
  `schemaFields()`; fixed.
- Draft mode: every component constructor and CRUD function takes
  `validate = FALSE`, building the classed object from incomplete input
  so
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
  can report what is missing. The app populates its SAP only this way.
  [`newSapDataSource()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/newSapDataSource.md)
  now accepts a missing `description`, as the schema always allowed.
- [`newSapId()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/newSapId.md)
  mints the next `<prefix>_<n>` id for a collection.
- Components are classed (`sap_cohort`, …) on every read path, including
  [`readSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/readSap.md).
- [`writeSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/writeSap.md)
  stamps `generated_at` at write time (`stamp = FALSE` keeps the
  object’s value).
