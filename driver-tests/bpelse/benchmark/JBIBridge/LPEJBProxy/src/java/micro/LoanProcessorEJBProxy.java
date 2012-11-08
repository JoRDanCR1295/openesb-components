/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package micro;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;
import org.netbeans.j2ee.wsdl.loanprocessorejbproxy.LoanProcessorEJBProxyPortType;
import stc.egate.jce.capsprojectloanprocessor_loanprocessor.CAPSProjectLoanProcessorLoanProcessorService;

/**
 *
 * @author mpottlapelli
 */
@WebService(serviceName = "LoanProcessorEJBProxyService", portName = "LoanProcessorEJBProxyPort", endpointInterface = "org.netbeans.j2ee.wsdl.loanprocessorejbproxy.LoanProcessorEJBProxyPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/LoanProcessorEJBProxy", wsdlLocation = "META-INF/wsdl/LoanProcessorEJBProxy/LoanProcessorEJBProxy.wsdl")
@Stateless
public class LoanProcessorEJBProxy implements LoanProcessorEJBProxyPortType {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/LoanProcessor/LoanProcessor.wsdl")
    private CAPSProjectLoanProcessorLoanProcessorService service;

    public void loanProcessorEJBProxyOperation(javax.xml.ws.Holder<org.netbeans.xml.examples.loanapplication.AutoLoanApplication> part1) {
        try { // Call Web Service Operation

            stc.egate.jce.capsprojectloanprocessor_loanprocessor.ExecutePortType port = service.getExecutePortType();
            javax.xml.ws.Holder<org.netbeans.xml.examples.loanapplication.AutoLoanApplication> body = new javax.xml.ws.Holder<org.netbeans.xml.examples.loanapplication.AutoLoanApplication>();
            body.value = part1.value;
            port.processLoan(body);
            part1.value = body.value;
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }
}
