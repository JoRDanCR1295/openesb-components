/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.pojose.tst;

import com.sun.tst.svc.Counter;
import java.util.logging.Logger;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.annotation.OnDone;
import org.glassfish.openesb.pojose.api.annotation.OnError;
import org.glassfish.openesb.pojose.api.annotation.OnReply;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.res.Context;

/**
 *
 * @author gpatil
 */
@Provider 
public class POJOTestRedeliveryThrottlingSvc {

    private enum TestType {Error, ErrorInOut, BpelError, Delete, Redirect, RedirectFromBPEL, RedirectInOut};
    private TestType testType = TestType.Error;
    private boolean gotError = false;
    private final Logger logger = Logger.getLogger(POJOTestRedeliveryThrottlingSvc.class.getName());
    
    @Resource
    private Context jbiCtx;

    @ConsumerEndpoint(serviceQN = "{http://j2ee.netbeans.org/wsdl/bplThrowError/throwError}epThrowErrorPortTypService", 
                      interfaceQN = "{http://j2ee.netbeans.org/wsdl/bplThrowError/throwError}throwErrorPortType",
                      name = "epThrowErrorPortTyp",
                      operationQN = "{http://j2ee.netbeans.org/wsdl/bplThrowError/throwError}throwErrorOperation",
                      inMessageTypeQN = "{http://j2ee.netbeans.org/wsdl/bplThrowError/throwError}throwErrorOperationRequest")
    private Consumer bpelErrorSvc;

    @ConsumerEndpoint(serviceQN = "{Test}RedirectableBPELSvcService",
        interfaceQN = "{http://j2ee.netbeans.org/wsdl/bplThrowError/throwError}throwErrorPortType",
        name ="RedirectedBPELSvcConsumer",
        operationQN = "{http://j2ee.netbeans.org/wsdl/bplThrowError/throwError}throwErrorOperation",
        inMessageTypeQN = "{http://j2ee.netbeans.org/wsdl/bplThrowError/throwError}throwErrorOperationRequest")
    private Consumer bpelRedirectTestSvc;

    @ConsumerEndpoint(serviceQN = "{Test}TstSvcService", 
            interfaceQN = "{http://svc.tst.sun.com/TstSvc/}TstSvcInterface",
            name = "TstSvcErrorConsumer")
    private Consumer pojoTestSvcError;

    @ConsumerEndpoint(serviceQN = "{Test}TstSvcService", 
            interfaceQN = "{http://svc.tst.sun.com/TstSvc/}TstSvcInterface",
            name = "TstSvcDeleteConsumer")
    private Consumer pojoTestSvcDelete;

    @ConsumerEndpoint(serviceQN = "{Test}RedirectableSvcService",
        interfaceQN = "{http://svc.tst.sun.com/TstSvc/}TstSvcInterface",
        name = "RedirectedSvcConsumer")
    private Consumer pojoTestSvcRedirect;

    @ConsumerEndpoint(serviceQN = "{Test}TstInOutSvcService",
            interfaceQN = "{http://svc.tst.sun.com/TstInOutSvc/}TstInOutSvcInterface",
            name = "TstInOutSvcErrorConsumer")
    private Consumer pojoTestInOutSvcError;

    @ConsumerEndpoint(serviceQN = "{Test}RedirectableInOutSvcService",
            interfaceQN = "{http://svc.tst.sun.com/TstInOutSvc/}TstInOutSvcInterface",
        name = "RedirectedInOutSvcConsumer")
    private Consumer pojoTestInOutSvcRedirect;
    
    private boolean gotInOutReply = false;

    public POJOTestRedeliveryThrottlingSvc() {    }
    
    @Operation (outMessageTypeQN="{http://tst.pojose.sun.com/POJOTestRedeliveryThrottlingSvc/}POJOTestRedeliveryThrottlingSvcOperationResponse")
    public String doTest(String input) {

        try {
            if (Counter.TestBPELErrorStr.equalsIgnoreCase(input)){
                testType = TestType.BpelError;
                bpelErrorSvc.sendInOut(input);
            } else if (Counter.TestErrorStr.equalsIgnoreCase(input)){
                testType = TestType.Error;
                Counter.TestErrorCounter.set(0);
                pojoTestSvcError.sendInOnly(input);
            }  else if (Counter.TestErrorInOutStr.equalsIgnoreCase(input)){
                testType = TestType.ErrorInOut;
                Counter.TestErrorCounter.set(0);
                pojoTestInOutSvcError.sendInOut(input);
            } else if (Counter.TestDeleteStr.equalsIgnoreCase(input)){
                testType = TestType.Delete;
                Counter.TestDeleteCounter.set(0);
                pojoTestSvcDelete.sendInOnly(input);
            } else if (Counter.TestRedirectFromBPELStr.equalsIgnoreCase(input)){
                testType = TestType.RedirectFromBPEL;
                bpelRedirectTestSvc.sendInOnly(input);
            } else if (Counter.TestRedirectStr.equalsIgnoreCase(input)){
                testType = TestType.Redirect;
                Counter.TestRedirectCounter.set(0);
                Counter.CalledRedirectedSvc.set(false);
                pojoTestSvcRedirect.sendInOnly(input);
            } else if (Counter.TestRedirectInOutStr.equalsIgnoreCase(input)){
                testType = TestType.RedirectInOut;
                Counter.TestRedirectCounter.set(0);
                Counter.CalledRedirectedInOutSvc.set(false);
                pojoTestInOutSvcRedirect.sendInOut(input);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        return input;        
    }

    @OnReply
    public void reply(ServiceEndpoint se, String ret){
        this.logger.info("Got reply:" + ret);
        
        if ((ret != null) && (ret.equalsIgnoreCase(Counter.TestRedirectInOutStr))){
            this.gotInOutReply = true;
        }
    }

    @OnError
    public void error(ServiceEndpoint se, Exception ex){
        this.gotError = true;
        this.logger.info("Got error:" + ex.getMessage());
    }

    @OnDone
    public String ret(){
        boolean retOk = false;
        if (this.testType.equals(TestType.Error)){
            if (this.gotError && (Counter.TestErrorCounter.get() == (5 + 1))){
                retOk = true;
            }
            return (retOk) ? ("Ok - " + Counter.TestErrorStr) : "NOk";
        } else if (this.testType.equals(TestType.ErrorInOut)){
            if (this.gotError && (Counter.TestErrorCounter.get() == (5 + 1))){
                retOk = true;
            }
            return (retOk) ? ("Ok - " + Counter.TestErrorStr) : "NOk";
        } else if (this.testType.equals(TestType.BpelError)){
            if (this.gotError){
                retOk = true;
            }
            return (retOk) ? ("Ok - " + Counter.TestBPELErrorStr) : "NOk";
        } else if (this.testType.equals(TestType.Delete)){
            if (Counter.TestDeleteCounter.get() == (5 + 1)){
                retOk = true;
            }
            return (retOk) ? ("Ok - " + Counter.TestDeleteStr) : "NOk";
        } else if (this.testType.equals(TestType.RedirectFromBPEL)){
            if ((Counter.CalledRedirectedSvc.get())){            
                retOk = true;
            }
            return (retOk) ? ("Ok - " + Counter.TestRedirectFromBPELStr) : "NOk";
        } else if (this.testType.equals(TestType.Redirect)){
            if ((Counter.TestRedirectCounter.get() == (5 + 1)) && (Counter.CalledRedirectedSvc.get())){
                retOk = true;
            }
            return (retOk) ? ("Ok - " + Counter.TestRedirectStr) : "NOk";
        } else if (this.testType.equals(TestType.RedirectInOut)){
            if ((Counter.TestRedirectCounter.get() == (5 + 1)) && 
                    (Counter.CalledRedirectedInOutSvc.get()) &&
                    (this.gotInOutReply)){
                retOk = true;
            }
            return (retOk) ? ("Ok - " + Counter.TestRedirectStr) : "NOk";
        } else {
            return (retOk) ? "Ok" : "NOk";
        }
    }
}