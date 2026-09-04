#include "ichiran_kernel.h"

#include <stdio.h>

int main(void) {
  const unsigned int actual = ichiran_kernel_abi_version();
  if (actual != ICHIRAN_KERNEL_ABI_VERSION) {
    fprintf(stderr, "Ichiran ABI mismatch: header=%u library=%u\n",
      ICHIRAN_KERNEL_ABI_VERSION, actual);
    return 1;
  }
  printf("Ichiran kernel ABI v%u\n", actual);
  return 0;
}
