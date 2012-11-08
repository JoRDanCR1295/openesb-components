/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.crpt;

import java.util.logging.Level;
import org.glassfish.openesb.pojose.api.annotation.*;
import java.util.logging.Logger;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.util.JAXBSource;
import javax.xml.transform.Source;
import org.netbeans.xml.schema.creditreport.CreditQuery;
import org.netbeans.xml.schema.creditreport.CreditReport;
import org.glassfish.openesb.pojose.api.res.POJOContext;
import org.glassfish.openesb.pojose.api.annotation.POJOResource;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.api.annotation.Endpoint;
import org.glassfish.openesb.pojose.api.res.POJOContext.MessageObjectType;

/**
 *
 * @author gpatil
 */
@POJO
public class POJOCreditReportSvcs {
    // logger
    private static final Logger logger = Logger.getLogger(POJOCreditReportSvcs.class.getName());
    JAXBContext jxbCtx = null;
    
    /**
     * Constructor
     */
    public POJOCreditReportSvcs() {
        try {
            this.jxbCtx = JAXBContext.newInstance(CreditReport.class.getPackage().getName());
        } catch (JAXBException ex) {
            Logger.getLogger(POJOCreditReportSvcs.class.getName()).log(Level.SEVERE, null, ex);
        }        
    }

    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation(outMessageTypeQN = "{http://crpt.sun.com/POJOCreditReportSvcs/}POJOCreditReportSvcsOperationResponse")
    public String receive(String input) {
            try {
                CreditReport cr = new CreditReport();
                CreditQuery cq = new CreditQuery();
                cq.setFirstName(input);
                cq.setLastName(input + "'s LastName");
                cq.setSsn("123-456-789");
                JAXBSource isrc = new JAXBSource(jxbCtx, cq);
                
                Source osrc = (Source) ctx.sendSynchInOut(sep1, isrc, MessageObjectType.Source);
                cr = (CreditReport) jxbCtx.createUnmarshaller().unmarshal(osrc);
                String output  = cr.getFirstName() + ":" + cr.getLastName() + ":" + cr.getSsn() + ":" + cr.getScore();
                return output;
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        
                
        return input;
    }
    @POJOResource
    private POJOContext ctx;
    @Endpoint(serviceQN = "{http://j2ee.netbeans.org/wsdl/bpelCreditReport/creditReport}ServiceEndpoint1226Service", inMessageTypeQN = "{http://j2ee.netbeans.org/wsdl/bpelCreditReport/creditReport}creditReportOperationRequest", operationQN = "{http://j2ee.netbeans.org/wsdl/bpelCreditReport/creditReport}creditReportOperation", name = "ServiceEndpoint1226", interfaceQN = "{http://j2ee.netbeans.org/wsdl/bpelCreditReport/creditReport}creditReportPortType")
    private ServiceEndpoint sep1;
}