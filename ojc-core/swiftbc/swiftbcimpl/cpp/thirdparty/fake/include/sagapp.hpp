#ifndef SAG_APP_H
#define SAG_APP_H
/*
#ifdef __cplusplus
extern "C" {
#endif
*/
#include <string>
using namespace std;
namespace SagApp{
  static int c_success = 0;
  static int c_transient = 1;
  static int c_logic = 2;
  static int c_fatal = 3;
  typedef long Token_t;
  class NamedItemList;
  class NamedItem{
	  string value;
	  NamedItemList * list;
	  string name;
  public:
    string & getItemValue();
    NamedItemList & getNamedItemList();
    void setItemName(string s);
    void setItemValue(string s);
    void setItemList(NamedItemList & l);
    string & getItemName();
    bool isItemList();
    
  };
  class NamedItemList{
	  NamedItem item;
  public:
	  NamedItemList(){
	  }
	  NamedItem & getNamedItem(int i){
		  return item;
	  }
	  NamedItem & getNamedItem(const char * c){
		  return item;
	  }
	  void insert(NamedItem & item){
	  }
	  void destroy(const char * s){
	  }
	  void destroy(int idx){
	  }
	  int getNumberOfItems(){
		  return 1;
	  }
  };
  class Envelope{
	  NamedItemList list;
	  string localAuth;
	  string appStatus;
	  string receiver;
	  string sender;
	  string msgFormat;
	  string applicationId;
	  string contextId;
	  string senderAuth;
	  string msgRef;
  public:
	  NamedItemList & getNamedItemList(){
		  return list;
	  }
	  string & getLocalAuth(){
		  return localAuth;
	  }
	  void setLocalAuth(string s){
		  localAuth = s;
	  }
	  string & getApplicationStatus(){
		  return appStatus;
	  }
	  void   setApplicationStatus(const char *  s){
		  appStatus = s;
	  }
	  string & getMsgRef(){
		  return msgRef;
	  }
	string & getReceiver(){
		return receiver;
	}
	void setReceiver(const char * s){
		receiver = s;
	}
	string & getSenderAuth(){
		return senderAuth;
	}
	void setSenderAuth(const char * s){
		senderAuth = s;
	}
	string & getSender(){
		return sender;
	}
	void setSender(const char * s){
		sender = s;
	}
	string & getMsgFormat(){
		return msgFormat;
	}
	void setMsgFormat(const char * s){
		msgFormat = s;
	}
	string & getApplicationId(){
		return applicationId;
	}
	void setApplicationId(const char * id){
		applicationId = id;
	}
	string & getContextId(){
		return contextId;
	}
	void setContextId(const char * id){
		contextId = id;
	}
  };
  class Letter{
	  const char * buf;
	  int length;
  public:
	  const char * getBuffer(){
		  return buf;
	  }
	  int getLength(){
		  return length;
	  }
	  void set(int len, const char * buffer){
		  buf = buffer;
		  length = len	;
	  }
  };
  class ExcStatus{
	  string code;
	  int severity;
	  string data;
	  string text;
	  string action;
	  string message;
	  Token_t token;
  public:
    
	  string & getCode(){
		  return code;
	  }
	  void setCode(const char * c){
		  code = c;
	  }
	  int getSeverity(){
		  return 0;
	  }
	  void setSeverity(int s){
		  severity = s;
	  }
	  string & getData(){
		  return data;
	  }
	  void setData(const char * d){
		data = d;
	  }
	  string & getText(){
		  return text;
	  }
	  void setText(const char * txt){
		  text = txt;
	  }
	  string & getAction(){
		  return action;
	  }
	  void setAction(const char * a){
		  action = a;
	  }
	  string & getMessage(){
		  return message;
	  }
	long getToken(){
		return token;
	}
	void setToken(Token_t t){
		token = t;
	}
  };
  class Message{
	  Envelope envelope;
	  Letter letter;
  public:
	  Envelope & getEnvelope(){
		  return envelope;
	  }
	  Letter & getLetter(){
		  return letter;
	  }
  };  class HandleServer{
  public:
	  HandleServer(){
	  }
	  long getRequest(Message request){
		  return 10;
	  }
	  long getRequest(long timeout, Message request){
		  return 20;
	  }
	  long getRequest(long timeout, Token_t token, Message request){
		  return 30;
	  }
	  void putResponse(Token_t token, Message response){
	  }
  };
  class HandleClient{
  public:
	  HandleClient(){
	  }
	  long putRequest(Message msg){
		  return 1;
	  }
	  long putRequest(Token_t token,Message msg){
		  return 2;
	  }
	  long getAnyResponse(Message response){
		  return 3;
	  }
	  long getAnyResponse(Token_t token,Message response){
		  return 4;
	  }
	  long getAnyResponse(long timeout, Token_t token,Message response){
		  return 5;
	  }
	  long getResponse(Token_t token, Message response){
		  return 6;
	  }
	  long getResponse(long timeout, Token_t token, Message response){
		  return 8;
	  }
	  void call(Message request, Message response){
	  }
  };
 
  class HandleFactory{
  public:
	  HandleServer * newHServer(const char * messagePartner){
		  return new HandleServer();
	  }
	HandleClient * newHClient(long timeout){
		return new HandleClient();
	}
	static HandleFactory *  newFactory(int args,const char * ar[2]){
		return new HandleFactory();
	}
  };
 
}
/*
#ifdef __cplusplus
}
#endif
*/
#endif
