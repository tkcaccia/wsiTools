# Project coordination instructions

These rules apply to work in this project and its job launchers.

## Mandatory HPC GPU policy

Before preparing, submitting, retrying or monitoring HPC/GPU work, read:
/Users/stefano/Documents/ChatGPT/Development Coordinator/docs/hpc-gpu-policy.md

- UCT HPC GPU work is paused until explicit user lifting AND HPC
  administrator approval. This includes retries, resumes, pilots, held
  controllers and CPU jobs/automations that can submit GPU descendants.
- On timeout, cancellation, abnormal exit or uncertain cleanup, fail the
  entire GPU bundle. Never launch its next GPU case, scorer or retry.
- Queue disappearance or a successful kill is not cleanup proof. Verify
  owned descendants/job steps and assigned GPU contexts with bounded
  checks; unknown cleanup means stop and escalate to administrators.
- Use finite shutdown/verification waits and bounded work units with
  atomic checkpoints before the scheduler deadline. Respect GPU allocations.
- Validate lifecycle failure cases on Chiamaka within approved scope first;
  its single GPU does not qualify multi-GPU or exact HPC shutdown behavior.
- Preserve active-job scripts, evidence and existing work. Do not reset GPUs
  or reboot nodes. Source/job changes still require explicit approval.


## Tissue visualizer skill maintenance

For any code change, run `python3 scripts/package_tissue_skill.py --check`.
If a watched wsiTools API changed, review `skills/wsi-tissue-visualizer/`
against the new behavior, update the skill as needed, then run
`python3 scripts/package_tissue_skill.py --sync-lock`. The pkgdown workflow
repackages the downloadable skill with a fresh source fingerprint on every
main-branch push. Do not refresh the API lock without reviewing compatibility.
