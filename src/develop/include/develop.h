#ifndef Develop_H
#define Develop_H


#ifdef DevelopVersion

#define speech_assert(s) //with assert and Rprintf there are problems, need other variant


#else

#define speech_assert(s)

#endif //DevelopVersion


#endif // Develop_H