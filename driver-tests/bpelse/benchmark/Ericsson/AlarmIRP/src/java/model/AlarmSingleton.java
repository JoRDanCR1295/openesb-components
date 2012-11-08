package model;
import java.util.ArrayList;
import java.util.List;

import model.AlarmInformation;


public class AlarmSingleton {
	private static List[][] alarmList=new List[4][4]; 
	static {
		for(int i=0;i<4;i++){
			for(int j=0;j<4;j++){
				int listSize=(int)Math.pow(10,i+1);
				alarmList[i][j]=new ArrayList<AlarmInformation>(listSize);
				for(int k=0;k<listSize;k++){
					AlarmInformation alarm=setUpAlarmInformation(j);
					alarmList[i][j].add(alarm);
				}
			}
		}
	}
	              
	private AlarmSingleton() {
        //buildAlarms();
    }
    
    
    public static AlarmInformation setUpAlarmInformation(int size){
    	AlarmInformation a=new AlarmInformation();
    	String alarmText="";
		if(size==1) alarmText="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
								   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
		if(size==2) alarmText="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
		if(size==3) alarmText="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"+
									"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
    	a.setAckState(true);
		a.setNotificationId(0);
		a.setAckTime(alarmText+getTime());
		a.setAckUserId(alarmText);
		a.setAlarmClearedTime(alarmText+getTime());
		a.setAlarmRaisedTime(alarmText+getTime());
		a.setEventType(alarmText);
		a.setProbableCause(alarmText);
		a.setAlarmChangedTime(alarmText+getTime());
		a.setSpecificProblem(alarmText);
		a.setAdditionalText(alarmText);
		a.setAdditionalInformation(alarmText);
		return a;
    }
    
    public static AlarmInformation[] getAlarmsBySeverity(int severity,int size) {
    	List<AlarmInformation> list=alarmList[severity][size];
    	return list.toArray(new AlarmInformation[getAlarmListSizeBySeverity(severity,size)]);
    }
    
       
    public static int getAlarmListSizeBySeverity(int severity,int size) {
        return alarmList[severity][size].size();
    }
    
    
    
    /*public static AlarmInformation get(int id){
    	return alarmList.get(id+"");
    }*/
    
    /*public static void acknowledge(int id,String acknowledger){
    	AlarmInformation ai=alarmList.get(id+"");
    	ai.setAckState(true);
    	ai.setAckUserId(acknowledger);
     }*/
    
    private static long getTime() {
        return System.currentTimeMillis();
    }
}
