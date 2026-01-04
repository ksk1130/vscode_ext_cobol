// client/src/logger.ts
import { configure, getConsoleSink, getLogger } from "@logtape/logtape";
import type { Logger } from "@logtape/logtape";

let isConfigured = false;

/**
 * Configure LogTape for the COBOL LSP client.
 * Should be called once at extension activation.
 */
export async function configureLogger(): Promise<void> {
    if (isConfigured) {
        return;
    }

    await configure({
        sinks: {
            console: getConsoleSink(),
        },
        filters: {},
        loggers: [
            { category: "cobol-lsp", lowestLevel: "debug", sinks: ["console"] },
        ],
    });

    isConfigured = true;
}

/**
 * Get a logger instance for the client.
 * @param category Optional subcategory
 * @returns Logger instance
 */
export function getClientLogger(category: string[] = []): Logger {
    return getLogger(["cobol-lsp", "client", ...category]);
}
