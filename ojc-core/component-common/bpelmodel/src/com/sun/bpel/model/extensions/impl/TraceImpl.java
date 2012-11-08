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
package com.sun.bpel.model.extensions.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.xml.namespace.QName;

import com.sun.bpel.model.extensions.Alert;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.impl.BPELElementImpl;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.NamespaceUtility;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.visitor.Visitor;

/*
 * @(#)TraceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
public class TraceImpl extends BPELElementImpl implements Trace {
	
	/* */
	private static final long serialVersionUID = -5467222970905881239L;

	/** QName object for SeeBeyond Private extension line label */
    public static final QName TRACE_QNAME = NamespaceUtility.getQName(
    		XMLElement.SBYNBPEL_RUNTIME_EXTN_NAMESPACE, Trace.TAG, XMLElement.SBYNBPEL_RUNTIME_EXTN_PREFIX);
        
	private ArrayList<Log> mLogs = new ArrayList<Log>();
	
	private ArrayList<Log> mOnStartLogs = new ArrayList<Log>();
	
	private ArrayList<Log> mOnCompleteLogs = new ArrayList<Log>();
	
	private ArrayList<Alert> mAlerts = new ArrayList<Alert>();
	
	private ArrayList<Alert> mOnStartAlerts = new ArrayList<Alert>();
	
	private ArrayList<Alert> mOnCompleteAlerts = new ArrayList<Alert>();
	
    /** Creates a new instance of TraceImpl */
    public TraceImpl() {
        super();
        initTrace();
    }
    
    /** Creates a new instance of TraceImpl.
     * @param   d   Owner document.
     */
    public TraceImpl(XMLDocument d) {
        super(d);
        initTrace();
    }
	
    /** Initializes this class.
     */
    private void initTrace() {
        setLocalName(Trace.TAG);
        setQualifiedName(TRACE_QNAME);
    	childrenTags = new String[] {
    			Log.TAG,
    			Alert.TAG
    	};
    }
    
    /** @see XMLNode#accept
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        
        if (!super.accept(v)) {
            return false;
        }
        
        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#addAlert(com.sun.bpel.model.extensions.Alert)
	 */
	public synchronized void addAlert(Alert alert) {
		super.addChild(2, alert);
		mAlerts.add(alert);
		if (alert.getLocation().equals(Trace.ON_START)) {
			mOnStartAlerts.add(alert);
		} else {
			mOnCompleteAlerts.add(alert);
		}
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#addLog(com.sun.bpel.model.extensions.Log)
	 */
	public synchronized void addLog(Log log) {
		super.addChild(1, log);
		mLogs.add(log);
		if (log.getLocation().equals(Trace.ON_START)) {
			mOnStartLogs.add(log);
		} else {
			mOnCompleteLogs.add(log);
		}
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#clearAlerts()
	 */
	public synchronized void clearAlerts() {
		
        for (int i = 0; i < mAlerts.size(); i++) {
        	super.removeChild(mAlerts.get(i));
        }
        
        mAlerts.clear();
        mOnStartAlerts.clear();
        mOnCompleteAlerts.clear();
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#clearLogs()
	 */
	public synchronized void clearLogs() {

        for (int i = 0; i < mLogs.size(); i++) {
        	super.removeChild(mLogs.get(i));
        }
        
        mLogs.clear();
        mOnStartLogs.clear();
        mOnCompleteLogs.clear();

	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getAlertSize()
	 */
	public int getAlertSize() {
		return mAlerts.size();
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getAlerts()
	 */
	public synchronized Collection<Alert> getAlerts() {
		return Collections.unmodifiableCollection((ArrayList<? extends Alert>) mAlerts.clone());
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getLogSize()
	 */
	public int getLogSize() {
		return mLogs.size();
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getLogs()
	 */
	public synchronized Collection<Log> getLogs() {
		return Collections.unmodifiableCollection((ArrayList<? extends Log>) mLogs.clone());
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getOnCompleteAlerts()
	 */
	public synchronized Collection<Alert> getOnCompleteAlerts() {
		return Collections.unmodifiableCollection((ArrayList<? extends Alert>) mOnCompleteAlerts.clone());
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getOnCompleteLogs()
	 */
	public synchronized Collection<Log> getOnCompleteLogs() {
		return Collections.unmodifiableCollection((ArrayList<? extends Log>) mOnCompleteLogs.clone());
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getOnStartAlerts()
	 */
	public synchronized Collection<Alert> getOnStartAlerts() {
		return Collections.unmodifiableCollection((ArrayList<? extends Alert>) mOnStartAlerts.clone());
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getOnStartLogs()
	 */
	public synchronized Collection<Log> getOnStartLogs() {
		return Collections.unmodifiableCollection((ArrayList<? extends Log>) mOnStartLogs.clone());
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#hasAlerts()
	 */
	public boolean hasAlerts() {
		return (mAlerts.size() > 0);
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#hasLogs()
	 */
	public boolean hasLogs() {
		return (mLogs.size() > 0);
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#hasOnCompleteAlerts()
	 */
	public boolean hasOnCompleteAlerts() {
		return (mOnCompleteAlerts.size() > 0);
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#hasOnCompleteLogs()
	 */
	public boolean hasOnCompleteLogs() {
		return (mOnCompleteLogs.size() > 0);
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#hasOnStartAlerts()
	 */
	public boolean hasOnStartAlerts() {
		return (mOnStartAlerts.size() > 0);
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#hasOnStartLogs()
	 */
	public boolean hasOnStartLogs() {
		return (mOnStartLogs.size() > 0);
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#removeAlert(com.sun.bpel.model.extensions.Alert)
	 */
	public synchronized boolean removeAlert(Alert alert) {
		super.removeChild(alert);
		mOnStartAlerts.remove(alert);
		mOnCompleteAlerts.remove(alert);
        return mAlerts.remove(alert);
	}

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#removeLog(com.sun.bpel.model.extensions.Log)
	 */
	public synchronized boolean removeLog(Log log) {
		super.removeChild(log);
		mOnStartLogs.remove(log);
		mOnCompleteLogs.remove(log);
        return mLogs.remove(log);
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getAlert(int)
	 */
	public Alert getAlert(int index) {
		return mAlerts.get(index);
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Trace#getLog(int)
	 */
	public Log getLog(int index) {
		return mLogs.get(index);
	}

}
