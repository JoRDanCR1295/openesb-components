/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ListenerMeta.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc;

import java.io.File;
import java.util.Properties;

/**
 * This is the class that encapsulates metadata
 * information about the message exchange listener
 * the call back for INONLY and INOUT message exchange
 * 
 * @author  jim.fu@sun.com
 *
 */
public class ListenerMeta {

    private long timestamp;
    private String msgTag;
    // the UUID tagged to msg file names
    // during the message transit
    private MessageExchangeReplyListener listener;
    // added for better post processing
    private String mPreDir;
    private String mPreFile;
    private String mTargetDir;
    private String mTargetFile;
    private String mPostDir;
    private String mPostFile;
    private File recoverLogEntry;
    private Properties mConnParams;
    // pre post commands
    private InboundMessageProcessor.PRE_POST_TRANSFER_CMDS mCommand;
    // sub binding type
    // MESSAGE for ftp:message
    // TRANSFER for ftp:transfer
    private InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES mSubBinding;

    public ListenerMeta(MessageExchangeReplyListener listener) {
        this(0, listener);
    }

    public ListenerMeta(long timestamp, MessageExchangeReplyListener listener) {
        this(timestamp, null, listener);
    }

    public ListenerMeta(long timestamp, String uuid, MessageExchangeReplyListener listener) {
        this.msgTag = uuid;
        this.timestamp = timestamp;
        this.listener = listener;
    }

    public String getMessageUUIDTag() {
        return msgTag;
    }

    public void setMessageUUIDTag(String uuid) {
        msgTag = uuid;
    }

    public long getRequestInvocationTime() {
        return this.timestamp;
    }

    public void setRequestInvocationTime(long t) {
        this.timestamp = t;
    }

    public void setPreDir(String dir) {
        mPreDir = dir;
    }

    public String getPreDir() {
        return mPreDir;
    }

    public void setPreFile(String file) {
        mPreFile = file;
    }

    public String getPreFile() {
        return mPreFile;
    }

    public void setDir(String dir) {
        mTargetDir = dir;
    }

    public String getDir() {
        return mTargetDir;
    }

    public void setFile(String file) {
        mTargetFile = file;
    }

    public String getFile() {
        return mTargetFile;
    }

    public void setPostDir(String dir) {
        mPostDir = dir;
    }

    public String getPostDir() {
        return mPostDir;
    }

    public void setPostFile(String file) {
        mPostFile = file;
    }

    public String getPostFile() {
        return mPostFile;
    }

    public InboundMessageProcessor.PRE_POST_TRANSFER_CMDS getCommand() {
        return mCommand;
    }

    public void setCommand(InboundMessageProcessor.PRE_POST_TRANSFER_CMDS cmd) {
        mCommand = cmd;
    }

    public InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES getSubBindingType() {
        return mSubBinding;
    }

    public void setSubBindingType(InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES subbinding) {
        mSubBinding = subbinding;
    }

    public void setRecoverLogEntry(File f) {
        recoverLogEntry = f;
    }

    public File getRecoverLogEntry() {
        return recoverLogEntry;
    }

    public Properties getConnectionParams() {
        return mConnParams;
    }

    public void setConnectionParams(Properties params) {
        mConnParams = params;
    }

    public MessageExchangeReplyListener getMessageExchangeReplyListener() {
        return this.listener;
    }
}
