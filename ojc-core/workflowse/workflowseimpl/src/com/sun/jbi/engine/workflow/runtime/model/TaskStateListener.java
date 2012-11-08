package com.sun.jbi.engine.workflow.runtime.model;

import javax.xml.namespace.QName;


public interface TaskStateListener {

	void onStateChange(TaskStateEvent evt);
    
    QName getTaskDefName ();

}
