/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.sample;


import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.xml.ws.BindingType;
import javax.xml.ws.Holder;

 

/**
 *
 * @author sbiswas
 */

@WebService(serviceName="HelloRpc" )
@BindingType(value="http://java.sun.com/xml/ns/jaxws/2003/05/soap/bindings/HTTP/")
@SOAPBinding(style=SOAPBinding.Style.RPC, use=SOAPBinding.Use.LITERAL, parameterStyle=SOAPBinding.ParameterStyle.WRAPPED)

public class HelloRpc {

    /**
     * Web service operation
     */
    @WebMethod(operationName = "test",action="urn:test")
    @WebResult(name="testReturn")
    public String test(@WebParam(name = "parameter")
    String parameter) {
        //TODO write your implementation code here:
        return "hello";
    }
    
     /**
     * Web service operation
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
     * Web service operation
     */
    @WebMethod(operationName = "test2")
    public String test2(@WebParam(name = "parameter")
    String parameter) throws SampleException {
       throw new SampleException("hello there");
    }


}
