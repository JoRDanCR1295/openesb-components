package com.sun.mock.participant2;

import javax.xml.namespace.QName;

public class ServiceConstants {

	public static String INPUT_PART_NAME = "part1";
	
	public static final String dataSourceJNDIName = "jdbc/__txdemo";
	
	public static final String PARTICIPANT1_IN_OUT_SERVICE_TNS = "urn:Participant1";
	
        public static final String PARTICIPANT2_IN_OUT_SERVICE_TNS = "urn:Participant2";
	
	public static QName PARTICIPANT1_IN_OUT_SERVICE_NAME = new QName(PARTICIPANT1_IN_OUT_SERVICE_TNS, "participant1InOutService");
	
	public static String PARTICIPANT1_IN_OUT_ENDPOINT_NAME = "InOutEndpoint";
	
	public static QName PARTICIPANT2_IN_OUT_SERVICE_NAME = new QName(PARTICIPANT2_IN_OUT_SERVICE_TNS, "participant2InOutService");
	
	public static String PARTICIPANT2_IN_OUT_ENDPOINT_NAME = "InOutEndpoint";
	
}
