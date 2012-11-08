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
 * @(#)$Id: BPELTestEventListener.java,v 1.3 2008/02/06 21:40:44 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpelmonitor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEventListener;
import com.sun.jbi.engine.bpel.core.bpel.util.EventProcessHelper;

public class BPELTestEventListener implements BPELEventListener {	
	File mOutputFile;

	public void init(EventProcessHelper eventHelper) {
		// TODO Auto-generated method stub
		String outputFile = System.getProperty("MONITOR.OUTPUT", System.getenv().get("HOME") + File.separator + "event.out");
		mOutputFile = new File (outputFile);
	}

	public void processEvent(BPELEvent event)  {
		// TODO Auto-generated method stub
		if (event.getEventType() == BPELEvent.EventType.BP_SYNCHRONIZE) {
			return;
		}
        FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(mOutputFile, true);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
        StringBuffer fileOutput = new StringBuffer();
        System.out.println(event.toString());
        fileOutput.append(event.toString());
        try {
			fos.write(fileOutput.toString().getBytes());
			fos.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        
	}

	public void resetProperties(Properties properties) {
		// TODO Auto-generated method stub

	}

    public void shutdown() {
    }


}
