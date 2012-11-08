/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package nz.co.telecom.pattern.demo.ejb.customer;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import org.netbeans.j2ee.wsdl.pattern_demo_ejb_customer.GetCustomerFault;
import org.netbeans.j2ee.wsdl.pattern_demo_ejb_customer.PatternDemoEjbCustomerPortType;
import org.netbeans.xml.schema.pattern_demo_ejb_customer.Customer;
import org.netbeans.xml.schema.pattern_demo_ejb_customer.CustomerFault;
import org.netbeans.xml.schema.pattern_demo_ejb_customer.GetCustomerRequest;

/**
 *
 * @author Alex
 */
@WebService(serviceName = "pattern-demo-ejb-customerService", portName = "pattern-demo-ejb-customerPort", endpointInterface = "org.netbeans.j2ee.wsdl.pattern_demo_ejb_customer.PatternDemoEjbCustomerPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/pattern-demo-ejb-customer", wsdlLocation = "META-INF/wsdl/CustomerService/pattern-demo-ejb-customer.wsdl")
@Stateless
public class CustomerService implements PatternDemoEjbCustomerPortType {

    public Customer getCustomer(GetCustomerRequest part1) throws GetCustomerFault {
        if (part1.getCustomerId() == 1) {
            Customer customer = new Customer();
            customer.setCustomerId(part1.getCustomerId());
            customer.setName("Gerald Smith");
            customer.setPhone("027-121212");
            XMLGregorianCalendar cal = getDatatypeFactory().newXMLGregorianCalendarDate(1980, 1, 1, DatatypeConstants.FIELD_UNDEFINED);
            customer.setDateOfBirth(cal);
            return customer;
        } else if (part1.getCustomerId() == 2) {
            Customer customer = new Customer();
            customer.setCustomerId(part1.getCustomerId());
            customer.setName("Gaius Julius Caesar");
            customer.setPhone("021-555555");
            XMLGregorianCalendar cal = getDatatypeFactory().newXMLGregorianCalendarDate(1900, 10, 1, DatatypeConstants.FIELD_UNDEFINED);
            customer.setDateOfBirth(cal);
            return customer;
        } else {
            CustomerFault fault = new CustomerFault();
            fault.setCode("12");
            fault.setMessage("The customer '" + part1.getCustomerId() + "' does not exist.");
            throw new GetCustomerFault("BOOM!", fault);
        }
    }

    private DatatypeFactory getDatatypeFactory() {
        try {
            return DatatypeFactory.newInstance();
        } catch (Exception e) {
            throw new IllegalStateException("JAXB DatatypeFactory cannot be instantiated.", e);
        }
    }

}
