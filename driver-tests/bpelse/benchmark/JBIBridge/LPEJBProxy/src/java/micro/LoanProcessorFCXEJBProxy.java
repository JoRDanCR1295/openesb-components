/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package micro;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;
import org.netbeans.j2ee.wsdl.loanprocessorfcxejbproxy.LoanProcessorFCXEJBProxyPortType;
import stc.egate.jce.capsprojectloanprocessorfcx_loanprocessor.CAPSProjectLoanProcessorFCXLoanProcessorService;

/**
 *
 * @author mpottlapelli
 */
@WebService(serviceName = "LoanProcessorFCXEJBProxyService", portName = "LoanProcessorFCXEJBProxyPort", endpointInterface = "org.netbeans.j2ee.wsdl.loanprocessorfcxejbproxy.LoanProcessorFCXEJBProxyPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/LoanProcessorFCXEJBProxy", wsdlLocation = "META-INF/wsdl/LoanProcessorFCXEJBProxy/LoanProcessorFCXEJBProxy.wsdl")
@Stateless
public class LoanProcessorFCXEJBProxy implements LoanProcessorFCXEJBProxyPortType {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/LoanProcessor_1/LoanProcessor.wsdl")
    private CAPSProjectLoanProcessorFCXLoanProcessorService service;

    public void loanProcessorFCXEJBProxyOperation(javax.xml.ws.Holder<org.netbeans.xml.examples.loanapplication.AutoLoanApplication> part1) {
        //TODO implement this method

        try { // Call Web Service Operation

            stc.egate.jce.capsprojectloanprocessorfcx_loanprocessor.ExecutePortType port = service.getExecutePortType();

            javax.xml.ws.Holder<org.netbeans.xml.examples.loanapplication.AutoLoanApplication> body = new javax.xml.ws.Holder<org.netbeans.xml.examples.loanapplication.AutoLoanApplication>();
            body.value = part1.value;
            port.processLoan(body);
            part1.value = body.value;
        } catch (Exception ex) {
           ex.printStackTrace();
        }
    }
}
