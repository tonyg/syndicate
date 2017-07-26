#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/ioctl.h>

#include <fcntl.h>
#include <err.h>
#include <errno.h>
#include <unistd.h>
#include <ifaddrs.h>

#include <net/if.h>
#include <net/ethernet.h>
#include <arpa/inet.h> /* for htons */

#include <pthread.h>

#include <net/if_arp.h>
#include <netpacket/packet.h>

static int lookupInterfaceInfo(int sock, char const *interfaceName, int info, struct ifreq *ifr) {
  strncpy(ifr->ifr_name, interfaceName, IFNAMSIZ);
  if (ioctl(sock, info, ifr) < 0) {
    perror("ioctl error while looking performing ioctl on interface");
    fprintf(stderr, "(ioctl number 0x%08x, interface %s)\n", info, interfaceName);
    return -1;
  } else {
    return 0;
  }
}

static int bindToInterface(int sock, char const *interfaceName) {
  struct ifreq ifr;
  struct sockaddr_ll socketAddress;

  if (lookupInterfaceInfo(sock, interfaceName, SIOCGIFINDEX, &ifr) < 0) {
    return -1;
  }

  socketAddress.sll_family = AF_PACKET;
  socketAddress.sll_protocol = htons(ETH_P_ALL);
  socketAddress.sll_ifindex = ifr.ifr_ifindex;

  if (bind(sock, (struct sockaddr *) &socketAddress, sizeof(socketAddress)) < 0) {
    perror("Bind error");
    return -1;
  }

  return 0;
}

static int openSocket(char const *interfaceName) {
  int sock = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
  if (sock < 0) {
    perror("Socket error");
    return -1;
  }

  if (bindToInterface(sock, interfaceName) == -1) {
    return -1;
  }

  return sock;
}

/* hwaddr should be of length ETH_ALEN */
static int socket_hwaddr(int sock, char const *interfaceName, char *hwaddr) {
  struct ifreq ifr;

  if (lookupInterfaceInfo(sock, interfaceName, SIOCGIFHWADDR, &ifr) < 0) {
    return -1;
  }

  if (ifr.ifr_hwaddr.sa_family != ARPHRD_ETHER) {
    return -1;
  }

  memcpy(hwaddr, ifr.ifr_hwaddr.sa_data, ETH_ALEN);
  return 0;
}

static void dump_row(long count, int numinrow, int *chs) {
  int i;

  printf("%08lX:", count - numinrow);

  if (numinrow > 0) {
    for (i = 0; i < numinrow; i++) {
      if (i == 8)
	printf(" :");
      printf(" %02X", chs[i]);
    }
    for (i = numinrow; i < 16; i++) {
      if (i == 8)
	printf(" :");
      printf("   ");
    }
    printf("  ");
    for (i = 0; i < numinrow; i++) {
      if (isprint(chs[i]))
	printf("%c", chs[i]);
      else
	printf(".");
    }
  }
  printf("\n");
}

static int rows_eq(int *a, int *b) {
  int i;

  for (i=0; i<16; i++)
    if (a[i] != b[i])
      return 0;

  return 1;
}

void dump_buffer_to_stdout(void *buf_v, int len, int hexmode) {
  unsigned char *buf = (unsigned char *) buf_v;
  long count = 0;
  int numinrow = 0;
  int chs[16];
  int oldchs[16];
  int showed_dots = 0;
  int i;

  if (hexmode) {
    for (i = 0; i < len; i++) {
      int ch = buf[i];

      if (numinrow == 16) {
	int i;

	if (rows_eq(oldchs, chs)) {
	  if (!showed_dots) {
	    showed_dots = 1;
	    printf("          .. .. .. .. .. .. .. .. : .. .. .. .. .. .. .. ..\n");
	  }
	} else {
	  showed_dots = 0;
	  dump_row(count, numinrow, chs);
	}

	for (i=0; i<16; i++)
	  oldchs[i] = chs[i];

	numinrow = 0;
      }

      count++;
      chs[numinrow++] = ch;
    }

    dump_row(count, numinrow, chs);

    if (numinrow != 0)
      printf("%08lX:\n", count);
  } else {
    fwrite(buf, 1, len, stdout);
    printf("\n");
    fflush(NULL);
  }
}

int main(int argc, char const *argv[]) {
  int handle = openSocket("eth0");
  uint8_t buf[65536];

  while (1) {
    ssize_t len = recv(handle, &buf[0], sizeof(buf), MSG_TRUNC);
    if (len == -1) {
      perror("recv");
      break;
    }

    uint8_t *ipbuf = buf + 14;

    uint32_t self_ip = 0x810a735e;

    uint32_t remote_ip = ntohl(*(int *)(&ipbuf[12]));
    uint32_t local_ip = ntohl(*(int *)(&ipbuf[16]));

    if (local_ip == self_ip) {
      printf("Got ping from %d.%d.%d.%d\n", ipbuf[12], ipbuf[13], ipbuf[14], ipbuf[15]);
      if ((len >= 28) && (ipbuf[9] == 1) && (ipbuf[20] == 8)) {
        ipbuf[20] = 0;
        {
          short *icmp_cksum = (short *) (&ipbuf[22]);
          *icmp_cksum = htons(ntohs(*icmp_cksum) + 0x0800);
        }
        *(int *)(&ipbuf[12]) = htonl(local_ip);
        *(int *)(&ipbuf[16]) = htonl(remote_ip);

        {
          uint8_t mac[6];
          memcpy(mac, buf, 6);
          memcpy(buf, buf+6, 6);
          memcpy(buf+6, mac, 6);
        }
        {
          ssize_t written = write(handle, buf, len);
          if (written != len) {
            perror("write");
            break;
          }
        }
      }
    }
  }

  return 0;
}
