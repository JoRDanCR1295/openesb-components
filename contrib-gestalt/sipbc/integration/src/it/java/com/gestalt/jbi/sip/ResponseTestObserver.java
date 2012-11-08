package com.gestalt.jbi.sip;

import javax.sip.ResponseEvent;
import javax.sip.header.CSeqHeader;
import javax.sip.message.Response;


public class ResponseTestObserver extends TestObserver {
    private int status = 0;

    public ResponseTestObserver(String method) {
        super(method);
    }

    public void update(Object arg) {
        if (arg instanceof ResponseEvent) {
            ResponseEvent re = (ResponseEvent) arg;
            Response response = re.getResponse();

            String method = ((CSeqHeader) response.getHeader(CSeqHeader.NAME)).getMethod();

            if (method.equals(this.method)) {
                this.status = response.getStatusCode();

                synchronized (this) {
                    notifyAll();
                }
            }
        }
    }

    public boolean wasNotified(int status) {
        return (status == this.status);
    }
}
