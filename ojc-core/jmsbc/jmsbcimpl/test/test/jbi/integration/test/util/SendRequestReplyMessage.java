package test.jbi.integration.test.util;

import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueConnectionFactory;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.jms.TextMessage;

import com.sun.jbi.jmsbc.util.GUIDUtil;

public class SendRequestReplyMessage {
	
	private String dest;
	private String replyTo;
	
	public SendRequestReplyMessage(String dest, String replyTo){
		setDest(dest);
		setReplyTo(replyTo);
	}

	public String getDest() {
		return dest;
	}

	public void setDest(String dest) {
		this.dest = dest;
	}

	public String getReplyTo() {
		return replyTo;
	}

	public void setReplyTo(String replyTo) {
		this.replyTo = replyTo;
	}

	public void sendMessageToQueue(String txtMsg) throws JMSException{
		QueueConnection qc = null;
		QueueSession qs = null;
		try{
			QueueConnectionFactory qcf = new com.sun.messaging.QueueConnectionFactory();
			qc = qcf.createQueueConnection();
			qs = qc.createQueueSession(false, Session.AUTO_ACKNOWLEDGE);
			Queue q = qs.createQueue(dest);
			Queue replyTo = qs.createQueue(this.replyTo);
			QueueSender sender = qs.createSender(q);
			TextMessage msg = qs.createTextMessage(txtMsg);
			msg.setJMSReplyTo(replyTo);
			sender.send(msg);
		}finally{
			if(qs != null){
				try{qs.close();}catch(Throwable t){
					t.printStackTrace();
				}
			}
			if(qc != null){
				try{qc.close();}catch(Throwable t){
					t.printStackTrace();
				}
			}
		}
		
	}
	
}
