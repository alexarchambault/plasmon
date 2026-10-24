import { defineConfig } from '@playwright/test'

export default defineConfig({
  testDir: 'tests',
  // Each worker runs its own code-server, Plasmon server, and build tool
  workers: process.env.CI ? 1 : 2,
  // The first test of a file usually imports a build, which can download a build tool
  timeout: 5 * 60_000,
  expect: { timeout: 60_000 },
  retries: 0,
  // Failing tests get a screenshot of the workbench and the end of the Plasmon log attached
  // (see lib/fixtures.ts), code-server logs are under test-results/
  reporter: process.env.CI ? [['list'], ['html', { open: 'never' }]] : 'list',
})
