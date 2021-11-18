import * as Sentry from "@sentry/browser";
import { Integrations } from "@sentry/tracing";

export async function sentry() {
  Sentry.init({
    dsn: "https://9b7eec08ea6745ae95f16d2ef13c2b41@o1068225.ingest.sentry.io/6062400",
    integrations: [new Integrations.BrowserTracing()],
    tracesSampleRate: 1.0,
  });
}
