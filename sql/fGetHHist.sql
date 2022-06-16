Use test
go

set statistics time on
go
--------------------------------------------------------------------
-- 
-- Consumes the test.dbo.Hotspot table and creates histograms for 
-- either the samples or the whole dataset
--
/*
IF OBJECT_ID(N'test.dbo.hotspothist', N'U') IS NOT NULL
    DROP TABLE test.dbo.hotspothist;
GO
select * 
into test.dbo.hotspothist
from dbo.fGetHotspotsE(.05, 1)
--*/--------------------------------------------------------------------
--
IF OBJECT_ID(N'fGetHHist', N'TF') IS NOT NULL
    DROP Function dbo.fGetHHist
GO
--
CREATE FUNCTION dbo.fGetHHist(@type varchar(30), @ptype varchar(30), @cohort int)
--------------------------------------------------------------------
-- dbo.fGetDensPD1(@N,@M)
-- 
-- Compute the densities of each cell type in the current Hotspot table
-- by both all cells and expression marker negative cells
--------------------------------------------------------------------
Returns @out table(
		sampleid  int,
		field_rank float, 
		tumor_area float,
		ptype_count float
	)
AS BEGIN
	---------------------------------------------------
	-- get the ptype from the @ptype variable
	---------------------------------------------------
	declare @db_ptype int = (select ptype from wsi.dbo.Phenotype where phenotype = @ptype)
	---------------------------------------------------
	-- compute porportional field rank
	---------------------------------------------------
	declare @h table (
		sampleid int,
		hpfid bigint,
		area float,
		r int,
		rtarea float,
		p float
	)
	IF @cohort = 0 
		BEGIN
			insert into @h
			select a.sampleid, a.hpfid, f.gprim.STArea() area, a.r,
				f.ganno.STIntersection(rt.RTanno).STArea() * 2e-7 as rtarea,
				cast(a.r as float) / count(*) over (PARTITION by a.sampleid) as p
				from test.dbo.hotspothist a, test.dbo.RTFrac rt, wsi.dbo.FieldGeometry f
				where a.hpfid = f.hpfid
				and a.hpfid = rt.hpfid
		END
	ELSE
		BEGIN
			insert into @h
			select a.sampleid, a.hpfid, f.gprim.STArea() area, a.r,
				f.ganno.STIntersection(rt.RTanno).STArea() * 2e-7 as rtarea,
				cast(a.r as float) / count(*) over (PARTITION by a.sampleid) as p
				from test.dbo.hotspothist a, test.dbo.RTFrac_vld rt, wsi13.dbo.FieldGeometry f
				where a.hpfid = f.hpfid
				and a.hpfid = rt.hpfid
		END
	---------------------------------------------------
	-- get the total ptype for the cohort
	---------------------------------------------------
	--declare @ptype_sum float = (
	--	select count(*)/ sum(h.area) ptype_sum
	--	from wsi.dbo.celltag a, @h h
	--	where h.hpfid = a.hpfid
	--	and a.ptype = @db_ptype
	--)
	---------------------------------------------------
	-- get the total ptype for each field increment by sample
	---------------------------------------------------
	declare @h2 table(
		sampleid int,
		field_rank float,
		tumor_area float,
		ptype_count float
	)
	--
	declare @h3 table(
		sampleid  int,
		field_rank float, 
		tumor_area float,
		ptype_count float
	)
	--
	if @cohort = 0
		BEGIN
			insert into @h2
			select a.sampleid,
				case when (@type = 'fields') THEN h.r ELSE h.p END as field_rank,
				rtarea as tumor_area,
				count(*) / (h.area * 2E-7) as ptype_count
				from wsi.dbo.celltag a, @h h
				where h.hpfid = a.hpfid
				and a.ptype = @db_ptype
				group by a.sampleid, rtarea, area,
					case when (@type = 'fields') THEN h.r ELSE h.p END
		---------------------------------------------------
		-- get the total ptype for each field increment across the cohort
		---------------------------------------------------
			insert into @h3
			select sampleid = 999, 
			case when (@type = 'fields') THEN h.r ELSE h.p END as field_rank,
			sum(rtarea) as tumor_area,
			count(*) / (sum(h.area) * 2E-7) as ptype_count
				from wsi.dbo.celltag a, @h h
				where h.hpfid = a.hpfid
				and a.ptype = @db_ptype
				group by case when (@type = 'fields') THEN h.r ELSE h.p END
		END
	ELSE
		BEGIN
			insert into @h2
			select a.sampleid,
				case when (@type = 'fields') THEN h.r ELSE h.p END as field_rank,
				rtarea as tumor_area,
				count(*) / (h.area * 2E-7) as ptype_count
				from wsi13.dbo.celltag a, @h h
				where h.hpfid = a.hpfid
				and a.ptype = @db_ptype
				group by a.sampleid, rtarea, area,
					case when (@type = 'fields') THEN h.r ELSE h.p END
		---------------------------------------------------
		-- get the total ptype for each field increment across the cohort
		---------------------------------------------------
			insert into @h3
			select sampleid = 999, 
			case when (@type = 'fields') THEN h.r ELSE h.p END as field_rank,
			sum(rtarea) as tumor_area,
			count(*) / (sum(h.area) * 2E-7) as ptype_count
				from wsi13.dbo.celltag a, @h h
				where h.hpfid = a.hpfid
				and a.ptype = @db_ptype
				group by case when (@type = 'fields') THEN h.r ELSE h.p END
		END
	---------------------------------------------------
	-- join two tables
	---------------------------------------------------
	insert into @out
	select * from @h2
	union all 
	select * from @h3
	--
--
return
--
END
GO
--
--/*
IF OBJECT_ID(N'test.dbo.hotspothist', N'U') IS NOT NULL
    DROP TABLE test.dbo.hotspothist;
GO
select * 
into test.dbo.hotspothist
from dbo.fGetHotspotsE(.05, 1)
--
select * from dbo.fGetHHist('props', 'CD8', 0)
order by sampleid, ptype_count

select * from dbo.fGetHHist('fields', 'CD8', 0)
order by sampleid, ptype_count
--
IF OBJECT_ID(N'test.dbo.hotspothist', N'U') IS NOT NULL
    DROP TABLE test.dbo.hotspothist;
GO
select * 
into test.dbo.hotspothist
from dbo.fGetHotspotsE_vld(.05, 1)
--
select * from dbo.fGetHHist('props', 'CD8', 13)
order by sampleid, ptype_count
select * from dbo.fGetHHist('fields', 'CD8', 13)
order by sampleid, ptype_count
--*/
