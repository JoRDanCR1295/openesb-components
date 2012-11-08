package model;

public class AlarmInformation {
    private int alarmId;
    private int notificationId;
    private String alarmRaisedTime;
    private String alarmClearedTime;
    private String eventType;
    private String probableCause;
    private int perceivedSeverity;
    private String ackTime = null;
    private String ackUserId = null;
    private boolean ackState = false;
    private String targetObject;
    private String additionalText;
    private String additionalInformation;
    private String alarmChangedTime;
    private String specificProblem;
    
    /** Creates a new instance of Alarm */
    public AlarmInformation() {
    }
    
    public AlarmInformation(int id,String event,int severity){
    	setAlarmId(id);
    	setEventType(event);
    	setPerceivedSeverity(severity);
    }

	public boolean isAckState() {
		return ackState;
	}

	public void setAckState(boolean ackState) {
		this.ackState = ackState;
	}

	public String getAckTime() {
		return ackTime;
	}

	public void setAckTime(String ackTime) {
		this.ackTime = ackTime;
	}

	public String getAckUserId() {
		return ackUserId;
	}

	public void setAckUserId(String ackUserId) {
		this.ackUserId = ackUserId;
	}

	public String getAlarmClearedTime() {
		return alarmClearedTime;
	}

	public void setAlarmClearedTime(String alarmClearedTime) {
		this.alarmClearedTime = alarmClearedTime;
	}

	public int getAlarmId() {
		return alarmId;
	}

	public void setAlarmId(int alarmId) {
		this.alarmId = alarmId;
	}

	public String getAlarmRaisedTime() {
		return alarmRaisedTime;
	}

	public void setAlarmRaisedTime(String alarmRaisedTime) {
		this.alarmRaisedTime = alarmRaisedTime;
	}

	public String getEventType() {
		return eventType;
	}

	public void setEventType(String eventType) {
		this.eventType = eventType;
	}

	public int getNotificationId() {
		return notificationId;
	}

	public void setNotificationId(int notificationId) {
		this.notificationId = notificationId;
	}

	public int getPerceivedSeverity() {
		return perceivedSeverity;
	}

	public void setPerceivedSeverity(int perceivedSeverity) {
		this.perceivedSeverity = perceivedSeverity;
	}

	public String getProbableCause() {
		return probableCause;
	}

	public void setProbableCause(String probableCause) {
		this.probableCause = probableCause;
	}

	public String getTargetObject() {
		return targetObject;
	}

	public void setTargetObject(String targetObject) {
		this.targetObject = targetObject;
	}

	public String getAdditionalInformation() {
		return additionalInformation;
	}

	public void setAdditionalInformation(String additionalInformation) {
		this.additionalInformation = additionalInformation;
	}

	public String getAdditionalText() {
		return additionalText;
	}

	public void setAdditionalText(String additionalText) {
		this.additionalText = additionalText;
	}

	public String getAlarmChangedTime() {
		return alarmChangedTime;
	}

	public void setAlarmChangedTime(String alarmChangedTime) {
		this.alarmChangedTime = alarmChangedTime;
	}

	public String getSpecificProblem() {
		return specificProblem;
	}

	public void setSpecificProblem(String specificProblem) {
		this.specificProblem = specificProblem;
	}

}