/** ASCII `ICHIPACK`. */
export const PACK_MAGIC = 'ICHIPACK';

/** The only container format version this package currently reads and writes. */
export const PACK_FORMAT_VERSION = 1;

/** Fixed header size in bytes. */
export const PACK_HEADER_BYTES = 32;

/** Fixed size of one section-directory entry in bytes. */
export const PACK_DIRECTORY_ENTRY_BYTES = 24;

/** Payload offsets are aligned so later readers can create typed-array views. */
export const PACK_SECTION_ALIGNMENT = 8;

export const PACK_MAX_SECTION_ID = 0xffff_ffff;
export const PACK_MAX_BYTE_LENGTH = 0xffff_ffff;
