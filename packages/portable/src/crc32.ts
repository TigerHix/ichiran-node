const CRC32_POLYNOMIAL = 0xedb8_8320;

const CRC32_TABLE = new Uint32Array(256);

for (let value = 0; value < CRC32_TABLE.length; value++) {
  let checksum = value;
  for (let bit = 0; bit < 8; bit++) {
    checksum = (checksum & 1) === 1
      ? CRC32_POLYNOMIAL ^ (checksum >>> 1)
      : checksum >>> 1;
  }
  CRC32_TABLE[value] = checksum >>> 0;
}

export function crc32(bytes: Uint8Array): number {
  let checksum = 0xffff_ffff;

  for (const byte of bytes) {
    checksum = CRC32_TABLE[(checksum ^ byte) & 0xff]! ^ (checksum >>> 8);
  }

  return (checksum ^ 0xffff_ffff) >>> 0;
}
