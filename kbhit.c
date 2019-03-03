#include <stdio.h>
//#include <termios.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>

int kbhit (void)
{
  struct timeval tv;
  fd_set rdfs;

  //set time value to zero timeout
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  //Initializes the file descriptor set rdfs to have zero bits for all file descriptors
  FD_ZERO(&rdfs);
  //Sets the bit for the STDIN file descriptor in the file descriptor set rdfs
  FD_SET (STDIN_FILENO, &rdfs);
  //Describes whether STDIN is ready for reading
  select(STDIN_FILENO+1, &rdfs, NULL, NULL, &tv);
  //Returns a non-zero value if the bit for the STDIN file descriptor is set in the
  // file descriptor set pointed to by rdfs, and 0 otherwise
  return FD_ISSET(STDIN_FILENO, &rdfs);

}

