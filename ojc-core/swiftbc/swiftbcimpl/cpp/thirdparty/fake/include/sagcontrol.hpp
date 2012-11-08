#ifndef SAG_CONTROL_H
#define SAG_CONTROL_H
/*
#ifdef __cplusplus
extern "C" {
#endif
*/
#include <string>
using namespace std;
namespace SagProcessControl{
  typedef long Token_t;
  class Header{
	  string userId;
	  string authentication;
	  string logApplicationId;
	  string primitive;
	  string toStr;
	  bool mode;
	  string applicationId;
  public:
	  Header(){
	  }
	  void setUserId(const char * sUserId){
		  userId = sUserId;
	  }
	  string & getUserId(){
		  return userId;
	  }
	  void setUserAuthentication(const char * sUserAuthentication){
		  authentication = sUserAuthentication;
	  }
	  string & getUserAuthentication(){
		  return authentication;
	  }
	  void setLogApplicationId(const char * sLogApplicationId){
		  logApplicationId = sLogApplicationId;
	  }
	string & getLogApplicationId(){
		return applicationId;
	}

	void setPrimitive(const char * sPrimitive){
		primitive = sPrimitive;
	}
	string & getPrimitive(){
		return primitive;
	}

	bool getCompatMode(){
		return mode;
	}

	string & toString(){
		return toStr;
	}

  };  class Primitive{
	  string text;
	  Header header;
  public:
	  string & getText(){
		  return text;
	  }
	  Header & getHeader(){
		  return header;
	  }
  };

  class Handle{
  public:
	  long putRequest(Token_t token,Primitive & p){
		  return 0;
	  }
	  void getResponse(Token_t token,Primitive & p){
	  }
	  long getAnyResponse(long timeout, Primitive & p){
		  return 0;
	  }
	  void getAnyResponse(long timeout, Token_t token, Primitive & p){
	  }
	  void getResponse(long timeout, Token_t token, Primitive & p){
	  }
	  void call(Primitive request, Primitive & response){
	  }
  };

  class HandleFactory{
  public:
	  static HandleFactory * newFactory(int argsCount, const char * chzArg[2]){
		  return new HandleFactory();
	  }
	  Handle * newHandle(){
		  return new Handle();
	  }
  };
};
/*
#ifdef __cplusplus
}
#endif
*/
#endif
