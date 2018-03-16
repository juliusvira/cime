!================================================================================
module shr_fan_mod

  use shr_kind_mod,only : r8 => shr_kind_r8
  use shr_kind_mod,only : CL => SHR_KIND_CL, CX => SHR_KIND_CX, CS => SHR_KIND_CS
  use shr_sys_mod, only : shr_sys_abort
  use shr_log_mod, only : loglev  => shr_log_Level
  use shr_log_mod, only : logunit => shr_log_Unit

  implicit none
  save
  private

contains

  subroutine shr_fan_readnl(filename_nl, id, fan_fields, have_fields)
    use shr_mpi_mod, only : shr_mpi_bcase
    character(len=*), intent(in)  :: filename_nl
    integer, intent(in) :: id ! seq_comm ID
    character(len=*), intent(out) :: fan_fields
    logical, intent(out) :: have_fields
    
    integer :: mpicomm, iostat, fileunit
    logical :: exists, fan_nh3_to_atm
    character(*),parameter :: subname = '(shr_fan_reanl) '

    namelist /fan_nh3_to_atm/ fan_nh3_to_atm
    
    call seq_comm_setptrs(id, mpicom=mpicomm)
    if (seq_comm_iamroot(id)) then
       inquire(file=trim(filename_nl), exist=exists)
       if (exists) then
          fileunit = shr_file_getUnit()
          open(fileunit, file=trim(filename_nl), status='old' )
          read(fileunit, fan_nh3_to_atm, iostat=iostat)
          if (iostat /= 0) then
             call shr_sys_abort(subName//'Error reading namelist')
          end if
          close(fileunit)
          call shr_file_freeunit(fileunit)
       end if
       call shr_mpi_bcase(fan_nh3_to_atm, mpicomm)
       have_fields = fan_nh3_to_atm
       if (fan_nh3_to_atm) then
          fan_fields = 'Fall_NH3FAN'
       else
          fan_fields = ''
       end if
  end subroutine shr_fan_readnl
  
endmodule shr_fan_mod
