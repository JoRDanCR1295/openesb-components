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
 * @(#)TaskInstance.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.hibernate;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.apache.solr.analysis.LowerCaseFilterFactory;
import org.apache.solr.analysis.TrimFilterFactory;
import org.hibernate.search.annotations.Analyzer;
import org.hibernate.search.annotations.AnalyzerDef;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.Field;
import org.hibernate.search.annotations.Index;
import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.annotations.Parameter;
import org.hibernate.search.annotations.Store;
import org.hibernate.search.annotations.TokenFilterDef;
import org.hibernate.search.annotations.TokenizerDef;

@AnalyzerDef(name="keywordsanalyzer",
        tokenizer = @TokenizerDef(factory = PatternWhiteSpaceTokenizer.class, params = {
        @Parameter(name="pattern", value= "(\\[)([^\\]]*)(\\])" ),
        @Parameter(name="group", value="2")}),
        filters = {
                @TokenFilterDef(factory = LowerCaseFilterFactory.class),
                @TokenFilterDef(factory = TrimFilterFactory.class)                
          }     
        
         )
         
@Indexed(index="indexes/taskInstance")
public class TaskInstance  implements Serializable {

    @DocumentId
    private Long id;

    private String taskDefId;

    private String messageExchangeId;
    
    @Field(name = "title", index=Index.TOKENIZED, store=Store.NO)
    @Analyzer(definition = "keywordsanalyzer")
    private String title;

    private Integer status;

    private Integer priority;

    private String owner;
    
    private String assignedTo;
  
    private String completedBy;

    private String failedCode;

    private String failedReason;
    
    private String inputData;

    private String outputData;

    private Date createDate;

    private Date endDate;
    
    private Date deadline;

    private Set<TaskAssignee> assignees = new HashSet<TaskAssignee>();
    
    private Set<TaskExcludedAssignee> excludedAssignees = new HashSet<TaskExcludedAssignee>();

    private Set<TaskTimer> timers = new HashSet<TaskTimer>();
    
    @Field(name = "keyword", index=Index.TOKENIZED, store=Store.NO)
    @Analyzer(definition = "keywordsanalyzer")
    private String keyword;

    public Date getCreateDate() {
        return createDate;
    }

    public void setCreateDate(Date createDate) {
        this.createDate = createDate;
    }
    
    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }    

    public String getFailedCode() {
        return failedCode;
    }  
    

    public Date getDeadline() {
        return deadline;
    }

    public void setDeadline(Date deadline) {
        this.deadline = deadline;
    }

    public void setFailedCode(String failedCode) {
        this.failedCode = failedCode;
    }

    public String getFailedReason() {
        return failedReason;
    }

    public void setFailedReason(String failedReason) {
        this.failedReason = failedReason;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getInputData() {
        return inputData;
    }

    public void setInputData(String inputData) {
        this.inputData = inputData;
    }

    public String getMessageExchangeId() {
        return messageExchangeId;
    }

    public void setMessageExchangeId(String messageExchangeId) {
        this.messageExchangeId = messageExchangeId;
    }

    public String getOutputData() {
        return outputData;
    }

    public void setOutputData(String outputData) {
        this.outputData = outputData;
    }

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public Integer getPriority() {
        return priority;
    }

    public void setPriority(Integer priority) {
        this.priority = priority;
    }

    public Integer getStatus() {
        return status;
    }

    public void setStatus(Integer status) {
        this.status = status;
    }

    public String getTaskDefId() {
        return taskDefId;
    }

    public void setTaskDefId(String taskDefId) {
        this.taskDefId = taskDefId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Set<TaskAssignee> getAssignees() {
        return assignees;
    }

    public void setAssignees(Set<TaskAssignee> assignees) {
        this.assignees = assignees;
    }

    public Set<TaskTimer> getTimers() {
        return timers;
    }

    public void setTimers(Set<TaskTimer> timers) {
        this.timers = timers;
    }

    @Override
    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((id == null) ? 0 : id.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final TaskInstance other = (TaskInstance) obj;
        if (id == null) {
            if (other.id != null)
                return false;
        } else if (!id.equals(other.id)) {
            return false;
        } else if (id.equals(other.id)) {
            return true;
        }
        return false;
    }

    public String getCompletedBy() {
        return completedBy;
    }

    public void setCompletedBy(String completedBy) {
        this.completedBy = completedBy;
    }

    public String getAssignedTo() {
        return assignedTo;
    }

    public void setAssignedTo(String assignedTo) {
        this.assignedTo = assignedTo;
    }

    public Set<TaskExcludedAssignee> getExcludedAssignees() {
        return excludedAssignees;
    }

    public void setExcludedAssignees(Set<TaskExcludedAssignee> excludedAssignees) {
        this.excludedAssignees = excludedAssignees;
    }

    public String getKeyword() {
        return keyword;
    }

    public void setKeyword(String keyword) {
        this.keyword = keyword;
    }

}
