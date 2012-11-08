package services.fm;
// Import the standard JWS annotation interfaces
import javax.ejb.Stateless;
import javax.jws.WebMethod;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;

import model.AlarmInformation;
import model.AlarmSingleton;

// Import the WebLogic-specific JWS annotation interface
//import javax.jws.HttpTransport;

// Standard JWS annotation that specifies that the portType name of the Web
// Service is "AlarmIRPPortType", its public service name is "AlarmIRP",
// and the targetNamespace used in the generated WSDL is "http://services/fm"
@WebService(serviceName="AlarmIRP", name="AlarmIRPPortType",
            targetNamespace="http://services/fm")
// Standard JWS annotation that specifies this is a document-literal-wrapped
// Web Service
@SOAPBinding(style=SOAPBinding.Style.DOCUMENT,
             use=SOAPBinding.Use.LITERAL,
             parameterStyle=SOAPBinding.ParameterStyle.WRAPPED)
// WebLogic-specific JWS annotation that specifies the context path and service
// URI used to build the URI of the Web Service is "alarmIRP/AlarmIRP"
//@WLHttpTransport(contextPath="alarmIRP", serviceUri="AlarmIRP",portName="AlarmIRPPort")

@Stateless()             
public class AlarmIRP {
	public static enum SEVERITY { Critical,Major,Minor,Warning }
	public AlarmIRP(){
	
	}

	@WebMethod(operationName="getVersion")
	public String getVersion() {
            return "1.1";
	}

	/*@WebMethod(operationName="getAlarmById")
	public AlarmInformation getAlarmById(@WebParam(name="Id") int id){
		AlarmInformation ai=AlarmSingleton.get(id);
		System.out.println(ai.toString());
		return ai;
	}*/
	
	/*@WebMethod(operationName="getAlarmStringById")
	public String getAlarmStringById(@WebParam(name="Id") int id){
		AlarmInformation ai=AlarmSingleton.get(id);
		String s= ai.toString();
		return s;
	}*/
	
	// 10 alarms
	
	@WebMethod(operationName="getAlarmsBySeverity10x1k")
	public AlarmInformation[] getAlarmsBySeverity10x1k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(0,0);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity10x2k")
	public AlarmInformation[] getAlarmsBySeverity10x2k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(0,1);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity10x5k")
	public AlarmInformation[] getAlarmsBySeverity10x5k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(0,2);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity10x10k")
	public AlarmInformation[] getAlarmsBySeverity10x10k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(0,3);
		return alarmList;
	}
	
	// 100 alarms
	
	@WebMethod(operationName="getAlarmsBySeverity100x1k")
	public AlarmInformation[] getAlarmsBySeverity100x1k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(1,0);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity100x2k")
	public AlarmInformation[] getAlarmsBySeverity100x2k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(1,1);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity100x5k")
	public AlarmInformation[] getAlarmsBySeverity100x5k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(1,2);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity100x10k")
	public AlarmInformation[] getAlarmsBySeverity100x10k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(1,3);
		return alarmList;
	}
	
	// 1000 alarms
	
	@WebMethod(operationName="getAlarmsBySeverity1000x1k")
	public AlarmInformation[] getAlarmsBySeverity1000x1k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(2,0);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity1000x2k")
	public AlarmInformation[] getAlarmsBySeverity1000x2k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(2,1);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity1000x5k")
	public AlarmInformation[] getAlarmsBySeverity1000x5k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(2,2);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity1000x10k")
	public AlarmInformation[] getAlarmsBySeverity1000x10k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(2,3);
		return alarmList;
	}
	
	// 10,000 alarms
	
	@WebMethod(operationName="getAlarmsBySeverity10000x1k")
	public AlarmInformation[] getAlarmsBySeverity10000x1k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(3,0);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity10000x2k")
	public AlarmInformation[] getAlarmsBySeverity10000x2k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(3,1);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity10000x5k")
	public AlarmInformation[] getAlarmsBySeverity10000x5k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(3,2);
		return alarmList;
	}
	
	@WebMethod(operationName="getAlarmsBySeverity10000x10k")
	public AlarmInformation[] getAlarmsBySeverity10000x10k(){
		AlarmInformation[] alarmList=AlarmSingleton.getAlarmsBySeverity(3,3);
		return alarmList;
	}

	/*@WebMethod(operationName="acknowledge")
	@Oneway
    public void acknowledge(@WebParam(name="Id") int id, @WebParam(name="AckUserId")String ackUserId) {
        AlarmSingleton.acknowledge(id,ackUserId);
    }*/
   
	@WebMethod(operationName="info1k")
    public String info1k() {
        return "This service holds (1k alarms)" + AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Critical.ordinal(),0) + " Critical, " 
                + AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Major.ordinal(),0) + " Major, " +
                AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Minor.ordinal(),0) + " alarms and " +
                AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Warning.ordinal(),0) + " warnings.";
    }
	
	@WebMethod(operationName="info2k")
    public String info2k() {
        return "This service holds (2k alarms)" + AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Critical.ordinal(),1) + " Critical, " 
                + AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Major.ordinal(),1) + " Major, " +
                AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Minor.ordinal(),1) + " alarms and " +
                AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Warning.ordinal(),1) + " warnings.";
    }
	
	@WebMethod(operationName="info5k")
    public String info5k() {
        return "This service holds (5k alarms)" + AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Critical.ordinal(),2) + " Critical, " 
                + AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Major.ordinal(),2) + " Major, " +
                AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Minor.ordinal(),2) + " alarms and " +
                AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Warning.ordinal(),2) + " warnings.";
    }
	
	@WebMethod(operationName="info10k")
    public String info10k() {
        return "This service holds (10k alarms)" + AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Critical.ordinal(),3) + " Critical, " 
                + AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Major.ordinal(),3) + " Major, " +
                AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Minor.ordinal(),3) + " alarms and " +
                AlarmSingleton.getAlarmListSizeBySeverity(SEVERITY.Warning.ordinal(),3) + " warnings.";
    }
}