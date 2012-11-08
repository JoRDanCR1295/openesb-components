/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.sample;

import java.util.logging.Logger;
import javax.jws.Oneway;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;
import javax.xml.ws.BindingType;
import javax.xml.ws.Holder;

/**
 *
 * @author sbiswas
 */
@WebService(serviceName="HelloService" )
@BindingType(value="http://java.sun.com/xml/ns/jaxws/2003/05/soap/bindings/HTTP/")
//@HandlerChain(file = "Hello_handler.xml")
public class Hello {
    
    private static Logger logger = Logger.getLogger("SOAP 1.2");

    /**
     * Web service operation
     */
    @WebMethod(action="hello/test" , operationName = "test")
    public String test(@WebParam(name = "parameter")
    String parameter) {
        String s = null;        
        //this will throw runtime exception
        s.toString();
 
        return "hello " + parameter;
    }

    /**
     * Web service operation
     * data coming as part of header and body, also make use of soap action
     */
    @WebMethod(action="hello/test1" ,operationName = "test1")
    public String test1(@WebParam(name = "myheader",header=true,mode=WebParam.Mode.INOUT)Holder<String> header,
                        @WebParam(name = "parameter")String parameter) throws SampleException {
        //TODO write your implementation code here:
        
        String ret = header.value + parameter;
        
        header.value="got it";
        
        return ret;
    }

    /**
     * throws user defined soap fault 
     */
    @WebMethod(operationName = "test2")
    public String test2(@WebParam(name = "parameter")
    String parameter) throws SampleException {
       throw new SampleException("hello there");
    }

    /**
     * one way operation
     */
    @WebMethod(operationName = "test3")
    @Oneway()
    public void test3(@WebParam(name = "parameter")
    String parameter) {
        logger.info("I am in oneway operation---" + parameter);
    }

    /**
     * use of the out mode
     */
    @WebMethod(operationName = "test4")
     public void test4(@WebParam(name = "parameter1") String parameter1,
                        @WebParam(name = "parameter2" , mode=WebParam.Mode.OUT)Holder<String> parameter2) {
        logger.info("I am only out---" + parameter2.value);
        parameter2.value="hello world";
    }
    
    
       @WebMethod(operationName = "addNumbers")
       @javax.xml.ws.Action(
          input="http://example.com/inputAction",
          output="http://example.com/outputAction",
          fault = {
              @javax.xml.ws.FaultAction(className=SampleException.class, value="http://example.com/faultAction")
          })
      public int addNumbers(int number1, int number2)throws SampleException {
          return number1 + number2;
      }


}
