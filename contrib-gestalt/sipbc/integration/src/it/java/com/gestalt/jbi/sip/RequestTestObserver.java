package com.gestalt.jbi.sip;

import java.util.logging.Logger;

import javax.sip.RequestEvent;
import javax.sip.message.Request;


public class RequestTestObserver extends TestObserver {
    private static final Logger log = Logger.getLogger(RequestTestObserver.class.getName());
    private String content = null;

    public RequestTestObserver(String method) {
        super(method);
    }

    public void update(Object arg) {
        if (arg instanceof RequestEvent) {
            RequestEvent re = (RequestEvent) arg;
            Request request = re.getRequest();

            if (request.getMethod().equals(this.method)) {
                byte[] content = request.getRawContent();

                if (content != null) {
                    this.content = new String(content);
                }

                synchronized (this) {
                    notifyAll();
                }
            }
        }
    }

    public boolean wasNotified(String content) {
        if ((content == null) || content.equals("")) {
            return this.content == null;
        } else {
            if (this.content == null) {
                return false;
            } else {
                return (content.equals(this.content));
            }
        }
    }
}
