package com.sun.soabi.snmpagent;

import java.net.UnknownHostException;


import com.sun.management.snmp.SnmpStatusException;
import com.sun.management.snmp.SnmpVarBindList;
import com.sun.management.snmp.manager.SnmpParameters;
import com.sun.management.snmp.manager.SnmpPeer;
import com.sun.management.snmp.manager.SnmpRequest;
import com.sun.management.snmp.manager.SnmpRequestHandler;
import com.sun.management.snmp.manager.SnmpSession;




public class TestSnmpGet {

	
	public static void main(String[] args) {
		
		try {
			
			SnmpSession session = new SnmpSession("test session");
			
			SnmpPeer peer = new SnmpPeer("localhost", 8085);
			SnmpParameters params = new SnmpParameters("public", "private");
			peer.setParams(params);
			
			
			SnmpVarBindList list = new SnmpVarBindList("var bind list");
			list.addVarBind("1.3.6.1.2.1.1.1.0");
			
			session.snmpGetRequest(peer, new AsynchRequestHandler(), list);
			
			
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SnmpStatusException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	
	static class AsynchRequestHandler implements SnmpRequestHandler {

		public void processSnmpInternalError(SnmpRequest request, String errmsg) {
			java.lang.System.out.println("requestHandler: error: " + errmsg);
			
		}

		public void processSnmpPollData(SnmpRequest request, int errStatus, int errIndex, SnmpVarBindList vblist) {
			java.lang.System.out.println("requestHandler: got data");
		}

		public void processSnmpPollTimeout(SnmpRequest request) {
			java.lang.System.out.println("requestHandler: timed out");
		}
		
	}
	
}
