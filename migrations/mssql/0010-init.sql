CREATE SCHEMA [dw]
GO
CREATE TABLE [dbo].[Client](
    [ClientID] [INT] IDENTITY(1,1) NOT NULL PRIMARY KEY,
    [ClientName] [NVARCHAR](255) NOT NULL
)
GO
CREATE TABLE [dw].[DayFetchQueue](
    [InstanceID] [INT] NOT NULL,
    [Year] [SMALLINT] NOT NULL,
    [Month] [TINYINT] NOT NULL,
    [Day] [TINYINT] NOT NULL,
    CONSTRAINT [PK_DayFetchQueue] PRIMARY KEY CLUSTERED
    (
        [InstanceID] ASC,
        [Year] ASC,
        [Month] ASC,
        [Day] ASC
    )
)
GO
CREATE TABLE [dw].[Instance](
    [InstanceID] [INT] IDENTITY(1,1) NOT NULL PRIMARY KEY,
    [ClientID] [INT] NOT NULL
        CONSTRAINT [FK_Instance_Client] FOREIGN KEY REFERENCES [dbo].[Client] ([ClientID]),
    [Domain] [NVARCHAR](255) NOT NULL,
    [Environment] [NVARCHAR](20) NOT NULL,
    [PollingEnabled] [BIT] NOT NULL
        CONSTRAINT [DF_Instance_PollingEnabled]  DEFAULT ((1)),
    [Username] [NVARCHAR](255) NOT NULL,
    [EncPassword] [VARBINARY](1000) NOT NULL
)
GO
CREATE TABLE [dw].[LogFile](
    [LogFileID] [INT] IDENTITY(1,1) NOT NULL PRIMARY KEY,
    [InstanceID] [INT] NOT NULL
        CONSTRAINT [FK_LogFile_Instance] FOREIGN KEY REFERENCES [dw].[Instance] ([InstanceID]),
    [FilePath] [NVARCHAR](255) NOT NULL,
    [BytesRead] [BIGINT] NOT NULL
        CONSTRAINT [DF_LogFile_BytesRead]  DEFAULT ((0)),
    [UtcServerLastModified] [DATETIME] NOT NULL,
    [UtcLastPolled] [DATETIME] NOT NULL
        CONSTRAINT [DF_Environment_UtcLastPolled]  DEFAULT (GETUTCDATE()),
    CONSTRAINT [UQ_LogFile_Env_FilePath] UNIQUE NONCLUSTERED
    (
        [InstanceID] ASC,
        [FilePath] ASC
    )
)
GO
CREATE TABLE [dw].[LogOmissionRegex](
    [LogOmissionRegexID] [INT] IDENTITY(1,1) NOT NULL PRIMARY KEY,
    [InstanceID] [INT] NOT NULL
        CONSTRAINT [FK_LogOmissionRegex_Instance] FOREIGN KEY REFERENCES [dw].[Instance] ([InstanceID]),
    [Regex] [NVARCHAR](255) NOT NULL,
    [Reason] [NVARCHAR](2000) NOT NULL
)
GO
