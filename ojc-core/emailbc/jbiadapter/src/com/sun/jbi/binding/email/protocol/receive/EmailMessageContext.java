/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.binding.email.protocol.receive;

import com.sun.jbi.binding.email.protocol.receive.MessageReceiver;
import com.sun.jbi.binding.email.I18n;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

/**
 * TODO: Need to do some sort of persistence here.
 * @author skini
 */
public class EmailMessageContext {
    private static final Logger logger = Logger.getLogger(EmailMessageContext.class.getName());
    
    private Set<String> readMessages = new HashSet<String>();
    private Set<String> doneMessages = new HashSet<String>();
    private Set<String> errorMessages = new HashSet<String>();
    private MessageReceiver receiver;

    public void setReceiver(MessageReceiver receiver) {
        this.receiver = receiver;
    }

    public synchronized void addDoneMessage(String messageId) {
        if (readMessages.remove(messageId)) {
            doneMessages.add(messageId);
        }
        I18n.finer(logger, "EMAILBC-2044: Received DONE ack for message: {0}", messageId);
        acknowledgeMessages();
    }

    public synchronized void addPendingMessage(String messageId) {
        readMessages.add(messageId);
        I18n.finer(logger, "EMAILBC-2043: Adding message to pending ack list: {0}", messageId);
    }

    public Set<String> getPendingMessages() {
        return readMessages;
    }

    public Set<String> getDoneMessages() {
        return doneMessages;
    }

    public Set<String> getErrorMessages() {
        return errorMessages;
    }

    public synchronized void removeMessageDone(String messageId) {
        doneMessages.remove(messageId);
    }

    public synchronized void addErrorMessage(String messageId) {
        if (readMessages.remove(messageId)) {
            errorMessages.add(messageId);
        }
        I18n.finer(logger, "EMAILBC-2045: Received ERROR ack for message:  {0}", messageId);
        acknowledgeMessages();
    }

    public synchronized void removeErrorMessage(String messageId) {
        errorMessages.remove(messageId);
    }

    public void acknowledgeMessages() {
        if (receiver != null) {
            receiver.doAcknowledge();
        }
    }

    public boolean hasAcknowledgeableMessages() {
        return !doneMessages.isEmpty() || !errorMessages.isEmpty();
    }
}
