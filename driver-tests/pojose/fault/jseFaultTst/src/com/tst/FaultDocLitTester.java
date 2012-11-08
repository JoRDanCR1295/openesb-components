/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.tst;

import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import org.glassfish.openesb.pojose.api.ErrorMessage;
import org.glassfish.openesb.pojose.api.FaultMessage;

/**
 *
 * @author gpatil
 */
@Provider (name="FaultDocLitTester",interfaceQN="{http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleDocLiteral}SimpleDocLiteralPortType",serviceQN="{http://tst.com/FaultDocLitTester/}FaultDocLitTesterService")
public class FaultDocLitTester {
    
    /**
     * Constructor
     */
    public FaultDocLitTester() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleDocLiteral}SimpleDocLiteralOperationResponse")
    public String doTest(String input) throws FaultMessage, ErrorMessage {
        if (input.contains("ThrowFaultUsingUtilMethod")){
            QName msgType = new QName("http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleDocLiteral", "SimpleDocLiteralOperationFaultMessage1");
            String abstractPayload = "<ns2:faultMessage xmlns:ns2=\"http://j2ee.netbeans.org/wsdl/caFaultTest/\"><msg>application fault message</msg></ns2:faultMessage>" ;
            FaultMessage fm = this.jbiCtx.createFaultMessage(abstractPayload, msgType);
            throw fm;
        } if (input.contains("ThrowErrorWithoutMessage")){
            ErrorMessage em = new ErrorMessage();
            throw em;
        } if (input.contains("ThrowErrorWithMessage")){
            ErrorMessage em = new ErrorMessage("HelloMessage");
            throw em;
        } if (input.contains("ThrowErrorWithException")){
            ErrorMessage em = new ErrorMessage(new Exception("HelloException"));
            throw em;
        } else {
            if (input != null){
                //System.out.println("Inp:" + input);
                input = input.replaceAll("inMessage", "outMessage");
                //System.out.println("Inp:" + input);
            }
        }
        return input;        
    }

    // Logger
    private static final Logger logger = Logger.getLogger(FaultDocLitTester.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;
}