#ifndef FAKE_SAG_APP_H
#define FAKE_SAG_APP_H
#include <sagapp.hpp>
#include <string>
using namespace std;
using namespace SagApp;
	
	string & NamedItem::getItemValue(){
		return value;
	}
    NamedItemList & NamedItem::getNamedItemList(){
		return (*list);
	}
    void NamedItem::setItemName(string s){
		name = s;
	}
    void NamedItem::setItemValue(string s){}
    void NamedItem::setItemList(NamedItemList & l){}
    string & NamedItem::getItemName(){
		return name;
	}
    bool NamedItem::isItemList(){
		return 1;
	}
    
	#endif
