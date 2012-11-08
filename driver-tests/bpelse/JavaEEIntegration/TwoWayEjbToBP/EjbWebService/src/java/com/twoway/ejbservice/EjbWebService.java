/*
 * EjbWebService.java
 * 
 * Created on Jul 9, 2007, 11:28:50 AM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.twoway.ejbservice;

import com.twoway.client.MsgType;
import com.twoway.client.TwoWaySrv;
import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;
import org.netbeans.j2ee.wsdl.ejbdriver.EjbPT;
import org.netbeans.xml.schema.ejbdriverschema.Mtype;

/**
 *
 * @author pvarghese
 */
@WebService(serviceName = "ejbSrv", portName = "ejbSrvPort", endpointInterface = "org.netbeans.j2ee.wsdl.ejbdriver.EjbPT", targetNamespace = "http://j2ee.netbeans.org/wsdl/EjbDriver", wsdlLocation = "META-INF/wsdl/EjbWebService/EjbDriver.wsdl")
@Stateless
public class EjbWebService implements EjbPT {
    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/TwoWay/TwoWay.wsdl")
    public TwoWaySrv service;

    public EjbWebService() {
    }

    public void ejbOper(javax.xml.ws.Holder<org.netbeans.xml.schema.ejbdriverschema.Mtype> msgPart) {
        //TODO implement this method
        Mtype inMsg =  msgPart.value;
        String inStr = inMsg.getStr() + "message in EJb: ";
        System.out.println("Input to EJb: " + inStr);

        try { // Call Web Service Operation
            com.twoway.client.TwoWayPT port = service.getTwoWaySrvPort();
             // TODO initialize WS operation arguments here
             MsgType callBpMsgType = new MsgType();
             callBpMsgType.setId(10);
             callBpMsgType.setMsg(inStr);
            javax.xml.ws.Holder<com.twoway.client.MsgType> twoWayPart = new javax.xml.ws.Holder<MsgType>();
            twoWayPart.value = callBpMsgType;
            System.out.println("EJb calling BP with: " + inStr);

            port.twoWayOper(twoWayPart);
            // return call value
            MsgType retVal = twoWayPart.value;
            String returnStrValFromBp = retVal.getMsg() + " return to soap client: ";
            Mtype outType = new Mtype();
            outType.setStr(returnStrValFromBp);
            System.out.println("EJb return to client: " + returnStrValFromBp);

            // should return the value to the soap client.
            msgPart.value = outType;
        } catch (Exception ex) {
            // TODO handle custom exceptions here
            System.out.println("The excep: " + ex);
        }


    }

}
