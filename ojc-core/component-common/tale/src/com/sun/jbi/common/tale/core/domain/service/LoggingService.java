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
 * @(#)LoggingService.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain.service;

import java.util.List;
import java.util.logging.Level;

import com.sun.jbi.common.tale.core.domain.TaleDomain;
import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.processor.AlerterChannelProcessor;
import com.sun.jbi.common.tale.core.domain.processor.ChannelProcessorChain;
import com.sun.jbi.common.tale.core.domain.processor.DBChannelProcessor;
import com.sun.jbi.common.tale.core.domain.processor.Processor;
import com.sun.jbi.common.tale.core.domain.processor.ThreadProcessor;
import com.sun.jbi.common.tale.core.domain.processor.TransactionProcessor;
import com.sun.jbi.common.tale.core.persist.dao.JpaDAO;
import com.sun.jbi.common.tale.core.persist.entity.AleChannels;
import com.sun.jbi.common.tale.core.persist.entity.CsfLoggerCodes;
import com.sun.jbi.common.tale.core.persist.entity.LoggerCodesChannel;
import com.sun.jbi.common.tale.core.util.I18n;
import com.sun.jbi.common.tale.core.util.TaleException;

/**
 * Implements logging functionality of ALE engine.
 * @author Kevan Simpson
 */
public class LoggingService extends AbstractTaleService {

    public LoggingService(TaleDomain domain) {
        super(domain);
    }
    
    /** @see com.sun.jbi.common.tale.core.domain.service.TaleService#execute(com.sun.jbi.common.tale.core.domain.TaleRequest) */
    public void execute(TaleRequest request) throws TaleException {
        JpaDAO dao = null;
        ChannelProcessorChain processorChain = new ChannelProcessorChain();
        try {
            dao = new JpaDAO();
            dao.initEntityManager();
            CsfLoggerCodes loggerCode = dao.getLoggerCodeRecord(request.getCode());
            if (loggerCode == null) {
                //Not a valid Logger Code..add to the dead letter queue??
                return;
            }            
            //To Do: Probably can cache ChannelProcessorChain for codes here??
            List channelList = dao.getChannelList(request.getCode());
            for (Object loggerChannel: channelList ) {                
                int channelCode = ((LoggerCodesChannel)loggerChannel).getLoggerCodesChannelPK().getAleChannelCode();
                AleChannels aleChannel = dao.getALEChannel(channelCode);
                if ("DATABASE".equals(aleChannel.getChannelType())) {
                    processorChain.addProcessor(new DBChannelProcessor());
                } else if ("ALERTER".equals(aleChannel.getChannelType())) {
                    processorChain.addProcessor(new AlerterChannelProcessor());
                }
            }
            
            //Create ThreadProcessor based on the configuration in loggerCode
            Processor processors = new ThreadProcessor(new TransactionProcessor(processorChain));
            //Execute all the processors
            processors.execute(request);
        }
        catch (Exception e) {
            String i18n = I18n.loc("TALE-6020: Failed to execute LoggingService: {0}", e.getMessage());
            log().log(Level.WARNING, i18n, e);
            throw new TaleException(i18n, e);
        }
        finally {
            dao.closeEntityManager();
        }
    }

}
