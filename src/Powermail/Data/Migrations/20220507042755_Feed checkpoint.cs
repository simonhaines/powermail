using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace Powermail.Data.Migrations
{
    public partial class Feedcheckpoint : Migration
    {
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.AddColumn<DateTime>(
                name: "Checkpoint",
                table: "UserFeeds",
                type: "TEXT",
                nullable: true);
        }

        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropColumn(
                name: "Checkpoint",
                table: "UserFeeds");
        }
    }
}
