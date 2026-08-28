# shinySAP schema history

The SAP JSON schema version recorded in a SAP's `sap_schema_version` field.
Independent of the package version in DESCRIPTION.

- **0.2.0** added cdm_sources and renamed analyses -> proposed_analyses.
- **0.3.0** moved the type-specific analysis fields under `parameters`.
- **0.3.1** made the Incidence parameters map 1:1 onto estimateIncidence().
- **0.4.0** added cohort sets (cohorts gained parent_cohort; prevalence gained
      denominatorCohortId / outcomeCohortId, null = all IDs in the set) and
      aligned prevalence with the estimators: parameters use the argument
      names and order, strata replaces stratifications, sensitivity_analyses
      is dropped there, and a prevalence analysis_type names the estimator
      (estimatePointPrevalence / estimatePeriodPrevalence).
- **0.4.1** dropped the denominator's declared strata_variables: the generator makes
      age_group and sex and nothing else, so they are fixed, not authored.
- **0.4.2** named every denominator key after the generator argument it feeds, and
      folded the two date keys into the one cohortDateRange pair it takes.
- **0.4.3** did the same for Incidence: its parameters are now flat and named exactly
      for estimateIncidence() (denominatorTable, outcomeTable, censorTable,
      *CohortId, interval, completeDatabaseIntervals, outcomeWashout,
      repeatedEvents, strata, includeOverallStrata), dropping the `estimand`
      wrapper and the snake_case keys.
- **0.4.4** trimmed cdm_sources to name / source_key / country (pickers now refer to
      sources by source_key), and in cdm_changes dropped cdm_version and
      replaced the scalar data_source with a data_sources array.
- **0.4.5** retyped cdm_changes around the questions a SAP asks of the CDM (extra
      validations, database-specific alterations, removing people with no year
      of birth or sex data). The old table-edit taxonomy described alterations,
      so legacy types map there and cdm_table/cdm_field fold into the
      description on load.
- **0.4.6** promoted the common alterations to change types of their own (subset a
      table, limit observation periods, remap concepts, implausible dates);
      unrecognised types land on "Other database-specific alteration".
- **0.4.7** dropped a cdm_change's rationale; an old one folds into the description.
- **0.4.8** gave study an amendments array (version, date, description of change).
- **0.4.9** gave study an aim (the research question introducing the numbered
      objectives; background doubles as rationale).
- **0.4.10** renamed study$acronym to study$study_code: what authors put there is a
       short study name or code, not an acronym.
- **0.4.11** dropped a cohort's cohort_id: IDs are assigned at generation time, not
       authored in the SAP. Without them the *CohortId sub-pickers stay hidden
       and those parameters serialise null = all cohorts in the set.
- **0.4.12** dropped a cohort's description: the kind block already says what a
       cohort is in structured form (entry events, criteria, generator
       arguments), and free text beside it only drifted out of step.
- **0.4.13** gave cohorts a data_sources array: the SAP-level counterpart of the
       generators' `cdm` argument, naming the CDM sources the cohort is built
       against -- the same shape analyses already used.
- **0.4.14** gave plain cohorts an index_rule: WHICH occurrence of the entry event
       indexes the cohort ("Index date" in protocol tables) -- previously
       conflated into the entry-event line.
- **0.4.15** folded index_rule and concept_set back into the text fields: the
       codelist is cited inline in the entry event that uses it, and the
       index rule is an inclusion criterion. Old values fold in on load.
- **0.4.16** added codelists: first-class entities (name, provenance, source file,
       and the codes themselves, uploaded from csv/txt/json) that cohorts
       cite in [square brackets]. The codes live in the SAP, so the plan is
       self-contained.
- **0.4.17** gave codelists an optional category (Index event, Comorbidity, ...):
       the document groups the codelist appendix by it, the way DARWIN SAPs
       do. Uncategorised codelists fall into "Other".
- **0.4.18** added study$min_cell_count, the omopgenerics::suppress(minCellCount =)
       threshold every result is suppressed at before export. Study-level, not
       per-analysis: it governs what may leave the data partner at all. A file
       without it loads at 5 -- the function's own default and the DARWIN
       convention -- rather than blank, which would read as no suppression.
- **0.4.19** stopped prefilling study$min_cell_count, and flags it when unset. The
       threshold is the data partners' governance decision, not a convention
       the app can confirm on their behalf -- omopgenerics defaults to 5, but
       DARWIN EU's own notice states 100 for the catalogue, so there is no one
       number to prefill. A file without the key now loads blank and stays
       blank; study_problems() reports it on Review so the gap is visible
       rather than papered over with a plausible guess.
- **0.4.20** made the concept set EXPRESSION canonical for a codelist, in
       omopgenerics' own field names (concept_id / excluded / descendants /
       mapped), and demoted `codes` to a resolved snapshot with an optional
       vocabulary_version stamp. The two answer different questions: an
       expression is the specification and resolves differently against each
       data partner's vocabulary, so a study running in five countries that
       shipped resolved codes would impose the authoring machine's vocabulary
       on all five. Speaking the standard's field names also means the
       generated study code can pass the expression straight to
       omopgenerics::newConceptSetExpression() with no translation step.
       A pre-0.4.20 file migrates losslessly: a flat list of concept ids is an
       expression with nothing excluded and no descendants.
- **0.4.21** gave a plain cohort optional typed `operations`: an ordered list of
       CohortConstructor verbs (see R/cohort_operations.R), which is what
       finally lets cohort code be generated rather than described. Free text
       is untouched and NEVER auto-converted -- prose does not carry the facts
       a call needs, which is the whole reason for the type. A cohort with
       operations renders its logic and its code from them; one without renders
       exactly as before.
- **0.4.22** gave each objective a stable {id, text} and let an analysis name the
       objectives it answers. Position was the only identity an objective had,
       and position is the one identity that cannot be referenced: inserting an
       objective silently repoints every reference below it. The link is
       many-to-many -- one objective is usually answered by several analyses
       (complete, 5-year and 2-year prevalence of the same disease) -- so an
       analysis carries a list. objective_coverage_problems() then reports an
       objective nothing implements and an analysis serving nothing, which is
       the check that makes an incomplete plan visible rather than inferable.
       A pre-0.4.22 file migrates losslessly: position WAS the identity, so
       ids are minted in order.
- **0.4.23** retired the `comparator` and `strata` cohort kinds. Neither is a cohort
       in an incidence-prevalence study: no estimator in IncidencePrevalence
       takes a comparator -- that is a comparative-cohort design, and a
       different package -- and strata are the fixed age_group / sex columns
       the denominator generator writes (STRATA_VARIABLES), chosen on the
       analysis, never defined as a cohort. Offering them invited a plan that
       could not be run. Nothing is lost on load: both kinds always rendered
       the plain-cohort block, and both now alias to `other`, which renders
       exactly the same fields.
- **0.4.24** cut the analysis-type dropdown to Incidence, Prevalence and Other. The
       seven it dropped -- cohort characterisation, comparative cohort,
       self-controlled case series, case-control, survival analysis,
       patient-level prediction, drug utilisation -- never had a template, so
       each already rendered the generic "Other" block and serialised the same
       generic fields. The label was the only thing that distinguished them,
       and it promised an estimator this app cannot generate; each would need
       its own package before it meant anything.
- **0.4.25** removed the migration layer: migrate_sap() and its six helpers, plus
       coalesce_key(). This app is pre-release and there are no SAPs in the
       wild, so every one of them was repairing a file that cannot exist. A
       file is now read as the current schema and nothing rewrites it on the
       way in.

       The entries above are kept as the RATIONALE for why each field is
       shaped the way it is -- that is still worth reading. Their "a pre-X
       file migrates losslessly" claims are not: nothing migrates any more.
       Treat them as history, not as behaviour.
- **0.4.26** finished the sweep 0.4.25 started, taking the legacy paths out of the
       templates themselves: the Incidence `estimand` wrapper and its
       snake_case renames, the Prevalence snake_case reads and the
       interval_length_days guess, the `stratifications` fallback, the
       top-level-parameters fallback in analysis_to_prefill(), the "unbounded"
       washout sentinel, the `role` cohort vocabulary, and the "Incidence rate"
       type alias. What is left in flatten() is its real job: the inverse of
       collect(), reconciling the few input ids that differ from their JSON
       keys, and defaulting the keys a type switch leaves absent.
- **0.4.27** gave an analysis `role` (primary / sensitivity): not an estimator
       argument -- 0.3.1 and 0.4.0 were right to drop `sensitivity_analyses`,
       since re-running with another washout is a second CALL -- but the
       distinction is the first thing a reviewer looks for, and without a field
       an author could only spell it into the analysis name, where nothing
       could group or check it. analysis_role_problems() reports a plan that
       states roles but marks none of them primary.

       No key changed for the other fix in this version, because the key was
       already there and simply ignored. `data_sources` -- WHICH databases a
       cohort is built in and an estimate runs against -- reached the document
       and stopped there, so a plan restricting an analysis to two of five
       databases generated a script that ran it at all five. That is the
       dangerous direction: the plan a reviewer signed said one thing and the
       code exported another. Restricted cohorts and estimates are now guarded
       with omopgenerics::cdmName(cdm), and cohort_source_coverage_problems()
       reports an estimate that runs where its cohort is never built -- the
       inconsistency that guard makes possible.

       The other three non-parameter fields -- name, role, objectives -- are
       now carried INTO the result, not just into a comment above the call.
       None of them is an estimator argument, because none is a computational
       choice: the primary analysis and its sensitivity analyses call the same
       function with the same arguments, and what separates them is which one
       the study may conclude from. omopgenerics settings are the documented
       home for that, so the generated script defines sapTag() and labels each
       estimate with sap_analysis / sap_role / sap_objectives. Verified against
       omopgenerics 1.4.1 and IncidencePrevalence 1.2.1: the columns survive
       bind(), suppress() and the export/import round trip, so a study report
       can tell the primary estimate from the other eight without going back
       to the plan.
- **0.4.28** added the `bind_cohorts` operation: several cohort definitions gathered
       into ONE table, so one estimator call covers all of them.

       estimatePrevalence()'s outcomeTable takes one table -- a vector fails
       with "You can only read one table of a cdm_reference" -- but a table may
       hold many cohorts and the estimator reports each separately. That is
       already why C1-001's six cancers are one analysis; what it could not do
       was combine definitions built by DIFFERENT pipelines.

       Only cohorts with disjoint NAMES can merge: bind() aborts on a
       duplicated cohort_name, and the estimator labels results by cohort_name,
       so two definitions under one name could never be told apart anyway.
       conceptCohort() names cohorts after their codelists, so definitions that
       differ only in exit (C1-001's three prevalence definitions) collide and
       stay separate analyses.

       The op is the first whose call returns a CDM REFERENCE rather than a
       cohort table, so register_cohort_op() gained `assigns`: "cdm" emits
       `cdm <- bind(...)`, "table" (the default, every CohortConstructor verb)
       emits `cdm$x <- ...`. Getting that wrong does not fail loudly -- it
       leaves the table unattached and every estimate on it dies at the data
       partner -- which is why it is declared per op rather than inferred.

       bound_cohort_problems() reports the four things the op's own validate()
       cannot see: binding a cohort nothing builds, one defined AFTER the bind,
       a denominator, or constituents whose tables share a cohort name.
- **0.4.29** took the codes back out of the SAP, reversing 0.4.16/0.4.20: a codelist
       is now {name, category, description} -- no upload, no
       concept_set_expression, no resolved `codes` snapshot, no source_file or
       vocabulary_version. The codes live with the study (the CSVs a codelist
       tool produces, placed under studyCode/codelist/), not in the plan, so
       the document's codelist-contents appendix is gone and the generated
       code reads or TODO-marks each cited codelist instead of inlining it.
- **0.4.30** cut the codelist categories to the three roles an incidence-prevalence
       study actually assigns -- Index event, Outcome, Covariate -- dropping
       Medicine / Procedure / Condition / Comorbidity (OMOP-domain-flavoured
       labels that said what the concepts WERE, not what the study uses them
       for) and replacing the explicit "Other" option with Covariate. Still
       free text for anything else; unset still renders under "Other".
