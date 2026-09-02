/* tslint:disable */
/* eslint-disable */

export class WasmDetailStore {
    free(): void;
    [Symbol.dispose](): void;
    entry_json(entry_index: number, compressed: Uint8Array): Uint8Array;
    constructor(prefix: Uint8Array, total_bytes: number);
    range_json(entry_index: number): Uint8Array;
    resident_bytes(): number;
}

export class WasmKernel {
    free(): void;
    [Symbol.dispose](): void;
    /**
     * The only analysis crossing: one UTF-16 input, one options document, and
     * one UTF-8 JSON result.
     */
    analyze_utf16_options(input: Uint16Array, options_json: Uint8Array): Uint8Array;
    entry_index_for_sequence(sequence: number): number;
    legacy_begin_utf16(input: Uint16Array, options_json: Uint8Array, method: string): WasmLegacyOperation;
    constructor(hot: Uint8Array);
    resident_payload_bytes(): number;
    romanize_utf16_options(input: Uint16Array, options_json: Uint8Array, method: string): Uint16Array;
}

export class WasmLegacyOperation {
    private constructor();
    free(): void;
    [Symbol.dispose](): void;
    /**
     * Returns a JSON envelope. `missing-detail` names the exact compressed
     * range the host must feed to `WasmDetailStore.entry_json` before retrying.
     */
    legacy_step(kernel: WasmKernel, details: WasmDetailStore): Uint8Array;
}

export function detail_prefix_length(header: Uint8Array, total_bytes: number): number;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly __wbg_wasmdetailstore_free: (a: number, b: number) => void;
    readonly __wbg_wasmkernel_free: (a: number, b: number) => void;
    readonly __wbg_wasmlegacyoperation_free: (a: number, b: number) => void;
    readonly detail_prefix_length: (a: number, b: number, c: number, d: number) => void;
    readonly wasmdetailstore_entry_json: (a: number, b: number, c: number, d: number, e: number) => void;
    readonly wasmdetailstore_open: (a: number, b: number, c: number, d: number) => void;
    readonly wasmdetailstore_range_json: (a: number, b: number, c: number) => void;
    readonly wasmdetailstore_resident_bytes: (a: number) => number;
    readonly wasmkernel_analyze_utf16_options: (a: number, b: number, c: number, d: number, e: number, f: number) => void;
    readonly wasmkernel_entry_index_for_sequence: (a: number, b: number, c: number) => void;
    readonly wasmkernel_legacy_begin_utf16: (a: number, b: number, c: number, d: number, e: number, f: number, g: number, h: number) => void;
    readonly wasmkernel_open: (a: number, b: number, c: number) => void;
    readonly wasmkernel_resident_payload_bytes: (a: number) => number;
    readonly wasmkernel_romanize_utf16_options: (a: number, b: number, c: number, d: number, e: number, f: number, g: number, h: number) => void;
    readonly wasmlegacyoperation_legacy_step: (a: number, b: number, c: number, d: number) => void;
    readonly __wbindgen_export: (a: number) => void;
    readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
    readonly __wbindgen_export2: (a: number, b: number) => number;
    readonly __wbindgen_export3: (a: number, b: number, c: number) => void;
    readonly __wbindgen_export4: (a: number, b: number, c: number, d: number) => number;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
 * Instantiates the given `module`, which can either be bytes or
 * a precompiled `WebAssembly.Module`.
 *
 * @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
 *
 * @returns {InitOutput}
 */
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
 * If `module_or_path` is {RequestInfo} or {URL}, makes a request and
 * for everything else, calls `WebAssembly.instantiate` directly.
 *
 * @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
 *
 * @returns {Promise<InitOutput>}
 */
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
