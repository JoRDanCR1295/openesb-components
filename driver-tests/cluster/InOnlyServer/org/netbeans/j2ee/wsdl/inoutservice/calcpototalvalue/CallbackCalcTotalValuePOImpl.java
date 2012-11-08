package org.netbeans.j2ee.wsdl.inoutservice.calcpototalvalue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.jws.WebService;
import javax.xml.ws.Endpoint;

import org.netbeans.xml.schema.poschema.PoType;

@WebService(endpointInterface = "org.netbeans.j2ee.wsdl.inoutservice.calcpototalvalue.CallbackCalcTotalValuePO",
            serviceName = "callbackCalcTotalValuePO")
public class CallbackCalcTotalValuePOImpl implements CallbackCalcTotalValuePO {
	static List<Long> msgList = Collections.synchronizedList(new ArrayList<Long>());

	public void getPO(PoType part1) {
		msgList.add(System.currentTimeMillis());
		synchronized (msgList) {
			if(msgList.size() >= 500){
				long duration = msgList.get(499) - msgList.get(0);
				System.out.println("Duration: " + duration +  ", Throughput: " + (500*1000)/duration);
				
				msgList.clear();
			}
		}
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
        System.out.println("Starting Server, it prints the throughput information for every 500 msgs");
        CallbackCalcTotalValuePOImpl implementor = new CallbackCalcTotalValuePOImpl();
        String address = "http://localhost:9000/callbackCalcTotalValuePO";
        Endpoint.publish(address, implementor);
        // END SNIPPET: publish
	}

}
