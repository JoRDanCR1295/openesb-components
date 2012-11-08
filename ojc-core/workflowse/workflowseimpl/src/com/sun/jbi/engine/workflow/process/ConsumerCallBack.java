package com.sun.jbi.engine.workflow.process;

import javax.xml.transform.dom.DOMSource;

import com.sun.jbi.engine.workflow.WorkflowMapEntry;

public interface ConsumerCallBack {

	void onNotify(WorkflowMapEntry entry, DOMSource reply);
}
