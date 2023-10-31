
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <string>
#include <termios.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include <sys/ioctl.h>


using namespace std;

// Input file descriptor for receiving requests from erlang
#define IN_DESC 3

// Output file descriptor for sending response to erlang
#define OUT_DESC 4

int init_port(string portname, int baudRate, int parity, int stopBits, int byteSize);
int from_to(int from,int to);
void reply(int erts, string result);

int main(int argc, char** argv) {
    // ---------Arguments-----------------
    string portname="/dev/ttyUSB0";
    int baudRate=9600;
    int parity = 0;
    int stopBits = 1;
    int byteSize = 8;
    for (int i = 0; i < argc; i++) {
        if (strcmp("-port", argv[i]) == 0) {
            if (i + 1 > argc) { reply(OUT_DESC, "ERROR: undefined port"); return 0; }
            portname = argv[i + 1];
        } else if (strcmp("-baudrate", argv[i]) == 0) {
            if (i + 1 > argc) { reply(OUT_DESC, "ERROR: undefined baudrate"); return 0; }
            baudRate = atoi(argv[i + 1]);
        } else if (strcmp("-parity", argv[i]) == 0) {
            if (i + 1 > argc) { reply(OUT_DESC, "ERROR: undefined parity"); return 0; }
            parity = atoi(argv[i + 1]);
        } else if (strcmp("-stopbits", argv[i]) == 0) {
            if (i + 1 > argc) { reply(OUT_DESC, "ERROR: undefined stopbits"); return 0; }
            stopBits = atoi(argv[i + 1]);
        } else if (strcmp("-bytesize", argv[i]) == 0) {
            if (i + 1 > argc) { reply(OUT_DESC, "ERROR: undefined bytesize"); return 0; }
            byteSize = atoi(argv[i + 1]);
        }
    }

    //printf("START PORT\n");
    //----------------Serial port init-------------------------
    int port=init_port(portname, baudRate, parity, stopBits, byteSize);
    if (port == -1) { reply(OUT_DESC, "ERROR: can not open port"); return 0; }
    reply(OUT_DESC, "OK");

    //------select for erts and serial-------------------------
    fd_set readfs;
    FD_ZERO(&readfs);
    int maxfd;
    maxfd = (IN_DESC > port ? IN_DESC : port) + 1;
    struct timeval Timeout;
    int result=1;


    //----------LOOP-------------------------
    while (result) {
        FD_SET(IN_DESC,&readfs);
        FD_SET(port,&readfs);
        Timeout.tv_usec = 0;
        Timeout.tv_sec  = 2;

        //printf("before select\r\n");
        select(maxfd, &readfs, NULL, NULL, &Timeout);
        //printf("after select\r\n");
        if (FD_ISSET(IN_DESC,&readfs)){
            //printf("send to\r\n");
            result=from_to(IN_DESC,port);
        }else if (FD_ISSET(port,&readfs)){
            result=from_to(port,OUT_DESC);
        }
    }
    return 0;
}


int from_to(int from,int to){
    int result=1;
    int bytes;
    ioctl(from, FIONREAD, &bytes);
    if (bytes==0){ return 0; }
    char buf[1024];
    int length;
    while (bytes>0){
        length=(bytes > 1024 ? 1024 : bytes);
        result=(read(from,buf,length)!=-1);
        if (!result) { break; }
        result=(write(to,buf,length)!=-1);
        if (!result) { break; }
        bytes=bytes-length;
    }
    return result;
}

int init_port(string portname, int baudRate, int parity, int stopBits, int byteSize){
    struct termios tio;
    int port = open(portname.c_str(), O_RDWR | O_NOCTTY | O_NDELAY);
    if (port==-1){ return -1; }

    bzero(&tio, sizeof(tio));

    //----wait lock for 1 byte-----
    tio.c_cc[VTIME] = 0;
    tio.c_cc[VMIN] = 1;

    //-----no owner change, CREAD?
    tio.c_cflag = CLOCAL | CREAD;

    //--------baudrate------------
    switch (baudRate) {
	case 50: tio.c_cflag |= B50; break;
	case 75: tio.c_cflag |= B75; break;
	case 110: tio.c_cflag |= B110; break;
	case 134: tio.c_cflag |= B134; break;
	case 150: tio.c_cflag |= B150; break;
	case 200: tio.c_cflag |= B200; break;
	case 300: tio.c_cflag |= B300; break;
	case 600: tio.c_cflag |= B600; break;
	case 1200: tio.c_cflag |= B1200; break;
	case 2400: tio.c_cflag |= B2400; break;
	case 4800: tio.c_cflag |= B4800; break;
	case 9600: tio.c_cflag |= B9600; break;
	case 19200: tio.c_cflag |= B19200; break;
	case 38400: tio.c_cflag |= B38400; break;
	case 57600: tio.c_cflag |= B57600; break;
	case 115200: tio.c_cflag |= B115200; break;
	case 230400: tio.c_cflag |= B230400; break;
	case 460800: tio.c_cflag |= B460800; break;
	case 500000: tio.c_cflag |= B500000; break;
	case 576000: tio.c_cflag |= B576000; break;
	case 921600: tio.c_cflag |= B921600; break;
	case 1000000: tio.c_cflag |= B1000000; break;
	case 1152000: tio.c_cflag |= B1152000; break;
	case 1500000: tio.c_cflag |= B1500000; break;
	case 2000000: tio.c_cflag |= B2000000; break;
	case 2500000: tio.c_cflag |= B2500000; break;
	case 3000000: tio.c_cflag |= B3000000; break;
	case 3500000: tio.c_cflag |= B3500000; break;
	case 4000000: tio.c_cflag |= B4000000; break;
	default: return -1;
    }

    //-------bytesize--------------
    switch (byteSize) {
	case 5: tio.c_cflag |= CS5; break;
	case 6: tio.c_cflag |= CS6; break;
	case 7: tio.c_cflag |= CS7; break;
	case 8: tio.c_cflag |= CS8; break;
	default: return -1;
    }

    switch (parity) {
	case 0:tio.c_iflag |= IGNPAR; break;
	case 1:	tio.c_cflag |= PARENB; tio.c_iflag |= INPCK | ISTRIP; break;
	case 2: tio.c_cflag |= PARENB | PARODD;	tio.c_iflag |= INPCK | ISTRIP; break;
    }

    switch (stopBits)	{
	case 1: break;
	case 2: tio.c_cflag |= CSTOPB; break;
    }

    tcflush(port, TCIFLUSH);
    tcsetattr(port, TCSANOW, &tio);
    return port;
}

void reply(int erts, string result) {
	size_t res = write(erts, result.c_str(),result.size());
    if (res == (size_t)-1){
        exit(1);
    }
}
