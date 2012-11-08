/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.event;

import com.sun.nvs.core.netabs.netelement.ne.IpAddress;

import java.net.InetAddress;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public class SyslogEvent extends NetEvent {
    private static final EventId unknown = new EventId("net.unknown.unknown");
    private static final String netEventPrefix = "net";
    private static final String serverEventPrefix = "server";
    private String _fullMessage;
    private boolean _decodeSuccess = false;
    private String _tag;
    private InetAddress _receivedFromIp = null;
    private IpAddress _source = null;
    private int _priority;
    private String _message;
    private long _receivedTime = System.currentTimeMillis();
    private String _module;
    private String _eventName;
    private long _seqNumber = 0L;

    /**
     * Creates a new SyslogEvent object.
     */
    public SyslogEvent() {
        super(Events.EVENT_TYPE_SYSLOG, Events.EVENT_SOURCE_TYPE_NETOBJECT);
    }

    /**
     * Creates a new SyslogEvent object.
     *
     * @param eveType DOCUMENT ME!
     * @param eveSourceType DOCUMENT ME!
     */
    public SyslogEvent(int eveType, int eveSourceType) {
        super(Events.EVENT_TYPE_SYSLOG, Events.EVENT_SOURCE_TYPE_LOCAL);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getFullMessage() {
        return _fullMessage;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String tag() {
        return _tag;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getReceivedTime() {
        return _receivedTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int priority() {
        return _priority;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int facility() {
        return _priority / 8;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int severity() {
        return _priority % 8;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String message() {
        return _message;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     */
    public void setFullMessage(String s) {
        _fullMessage = s;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isDecodeSuccess() {
        return _decodeSuccess;
    }

    /**
     * DOCUMENT ME!
     *
     * @param val DOCUMENT ME!
     */
    public void setDecodeSuccess(boolean val) {
        _decodeSuccess = val;
    }

    /**
     * DOCUMENT ME!
     *
     * @param rxIp DOCUMENT ME!
     */
    public void setSourceIp(InetAddress rxIp) {
        _receivedFromIp = rxIp;

        if (rxIp != null) {
            _source = new IpAddress(rxIp);
        } else {
            _source = null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public InetAddress getSourceIp() {
        return _receivedFromIp;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getSource() {
        return _source;
    }

    /**
     * DOCUMENT ME!
     *
     * @param time DOCUMENT ME!
     */
    public void setReceivedTime(long time) {
        _receivedTime = time;
    }

    /**
     * DOCUMENT ME!
     *
     * @param i DOCUMENT ME!
     */
    public void setPriority(int i) {
        _priority = i;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     */
    public void setTag(String s) {
        setTag(s, netEventPrefix);
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param prefix DOCUMENT ME!
     */
    public void setTag(String s, String prefix) {
        _tag = s;

        if (s == null) {
            setId(unknown);

            return;
        }

        _module = "unknown";
        _eventName = "unknown";

        try {
            if (s.startsWith("%")) {
                s = s.substring(1);
            }

            int idx = s.indexOf("-");

            if (idx <= 0) {
                return;
            }

            _module = s.substring(0, idx);
            s = s.substring(idx + 1);
            idx = s.indexOf("-");
            _eventName = s.substring(idx + 1);
        } catch (Exception e) {
        }

        setId(new EventId(prefix + "." + _module + "." + _eventName));
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getModule() {
        return _module;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     */
    public void setMessage(String s) {
        _message = s;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getSeqNumber() {
        return _seqNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param sn DOCUMENT ME!
     */
    public void setSeqNumber(long sn) {
        _seqNumber = sn;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        StringBuffer sb = new StringBuffer();

        if (_decodeSuccess) {
            sb.append("Syslog Message {\n");
            sb.append("\tDecode Status: OK\n");
            sb.append("\tReceived At: " + (new java.util.Date(_receivedTime)) + "\n");

            if (_receivedFromIp != null) {
                sb.append("\tFrom: " + _receivedFromIp.getHostAddress() + "\n");
            }

            sb.append("\tMessage: " + _message + "\n");
            sb.append("\tTag: " + _tag + "\n");
            sb.append("\tPriority: " + priority() + "\n");
            //sb.append("\tSeverity: "+severity()+"\n");
            sb.append("}\n");
        } else {
            sb.append("Syslog Message {\n");
            sb.append("\tDecode Status: Failed\n");
            sb.append("\tReceived At: " + (new java.util.Date(_receivedTime)) + "\n");

            if (_receivedFromIp != null) {
                sb.append("\tFrom: " + _receivedFromIp.getHostAddress() + "\n");
            }

            sb.append("\tFullMessage: " + _fullMessage + "\n");
            sb.append("}\n");
        }

        return sb.toString();
    }
}
