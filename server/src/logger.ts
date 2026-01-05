// server/src/logger.ts
import { configure, getConsoleSink, getLogger } from "@logtape/logtape";
import type { Logger } from "@logtape/logtape";

let isConfigured = false;

/**
 * Configure LogTape for the COBOL LSP server.
 * Should be called once at server startup.
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
 * Get a logger instance for the specified category.
 * @param category Logger category (e.g., "server", "resolver", "index")
 * @returns Logger instance
 */
export function getServerLogger(category: string[] = []): Logger {
    return getLogger(["cobol-lsp", "server", ...category]);
}

/**
 * Get a logger for resolver modules.
 * @param name Resolver name
 * @returns Logger instance
 */
export function getResolverLogger(name: string): Logger {
    return getLogger(["cobol-lsp", "resolver", name]);
}

/**
 * Get a logger for index modules.
 * @param name Index name
 * @returns Logger instance
 */
export function getIndexLogger(name: string): Logger {
    return getLogger(["cobol-lsp", "index", name]);
}
