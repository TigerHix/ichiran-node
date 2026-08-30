/** Metadata stored for one section in a portable pack. */
export interface PackSection {
  /** Stable, non-zero identifier owned by the pack producer. */
  readonly id: number;
  /** Byte offset of the section payload from the start of the pack. */
  readonly offset: number;
  /** Number of payload bytes. */
  readonly byteLength: number;
  /** CRC-32 of the payload. */
  readonly checksum: number;
}

/** Parsed metadata for a complete portable pack. */
export interface PackManifest {
  readonly formatVersion: number;
  readonly byteLength: number;
  readonly sections: readonly PackSection[];
}

/** Input accepted by {@link encodePack}. */
export interface PackSectionInput {
  /** Stable, non-zero identifier owned by the pack producer. */
  readonly id: number;
  /** Payload copied into the encoded pack. */
  readonly bytes: ArrayBuffer | Uint8Array;
}

export type PackFormatErrorCode =
  | 'invalid-input'
  | 'invalid-header'
  | 'unsupported-version'
  | 'invalid-directory'
  | 'corrupt-section'
  | 'missing-section';
