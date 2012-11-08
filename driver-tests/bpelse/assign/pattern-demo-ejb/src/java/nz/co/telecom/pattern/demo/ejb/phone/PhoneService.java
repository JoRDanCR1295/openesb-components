/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package nz.co.telecom.pattern.demo.ejb.phone;

import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.pattern_demo_ejb_phone.GetPhoneFault;
import org.netbeans.j2ee.wsdl.pattern_demo_ejb_phone.PatternDemoEjbPhonePortType;
import org.netbeans.xml.schema.pattern_demo_ejb_phone.GetPhoneRequest;
import org.netbeans.xml.schema.pattern_demo_ejb_phone.Phone;
import org.netbeans.xml.schema.pattern_demo_ejb_phone.PhoneFault;


/**
 *
 * @author Alex
 */
@WebService(serviceName = "pattern-demo-ejb-phoneService", portName = "pattern-demo-ejb-phonePort", endpointInterface = "org.netbeans.j2ee.wsdl.pattern_demo_ejb_phone.PatternDemoEjbPhonePortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/pattern-demo-ejb-phone", wsdlLocation = "META-INF/wsdl/PhoneService/pattern-demo-ejb-phone.wsdl")
@Stateless
public class PhoneService implements PatternDemoEjbPhonePortType {

    public Phone getPhone(GetPhoneRequest part1) throws GetPhoneFault {
        if ((part1.getPhoneId() != null && part1.getPhoneId() == 1) || "027-121212".equals(part1.getPhoneNumber())) {
            Phone phone = new Phone();
            phone.setPhoneId(1);
            phone.setPhoneNumber("027-121212");
            phone.setNetwork("Telecom");
            return phone;
        } else {
            PhoneFault fault = new PhoneFault();
            fault.setCode("12");
            fault.setMessage("The phone '" + part1.getPhoneId() + "' with number '" + part1.getPhoneNumber() + "' does not exist.");
            throw new GetPhoneFault("BOOM!", fault);
        }
    }

}
